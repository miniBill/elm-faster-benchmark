module Frontend.App exposing (Flags, Model, Msg, Ports, Program, RunStatus, app)

import Backend.Benchmark exposing (Stats)
import Browser
import Codec exposing (Codec, Value)
import Color
import Common.Types as Types exposing (Config, Index, Param, ToBackend(..), ToFrontend(..))
import Deque exposing (Deque)
import Dict exposing (Dict)
import Element exposing (Element, centerY, column, el, height, px, row, text, width, wrappedRow)
import Element.Background as Background
import Frontend.LinePlot
import Frontend.OkLch
import Frontend.Theme as Theme
import Frontend.Update as Update
import Frontend.WorkerQueue as WorkerQueue exposing (WorkerQueue)


type alias Ports msg =
    { terminateAll : {} -> Cmd msg
    , fromBackend : ({ index : Int, data : Value } -> msg) -> Sub msg
    , toBackend : { index : Int, data : Value } -> Cmd msg
    }


type alias Flags =
    { workersCount : Int
    }


type alias Model graph function =
    { workers : WorkerQueue
    , queue : Deque (Param graph function)
    , runStatus : RunStatus graph function
    }


type alias Results =
    Dict GraphName (Dict FunctionName (Dict Int Stats))


type alias GraphName =
    String


type alias FunctionName =
    String


type RunStatus graph function
    = LoadingParams
    | Ready (Inputs graph function)
    | Running (Inputs graph function) Results
    | Finished (Inputs graph function) Results
    | Stopped (Inputs graph function) Results


type Msg graph function
    = FromBackend Index (ToFrontend graph function)
    | Start (Inputs graph function)
    | Stop (Inputs graph function) Results
    | Nop


type alias Inputs graph function =
    { timeout : Maybe Float
    , params : List (Param graph function)
    }


type alias Program graph function =
    Platform.Program Flags (Model graph function) (Msg graph function)


app : Config graph function -> Ports (Msg graph function) -> Program graph function
app config ports =
    Browser.element
        { init = init config ports
        , view = \model -> Element.layout [] <| view model
        , update = update config ports
        , subscriptions =
            let
                subs : Sub (Msg graph function)
                subs =
                    subscriptions config ports
            in
            \_ -> subs
        }


init : Config graph function -> Ports (Msg graph function) -> Flags -> ( Model graph function, Cmd (Msg graph function) )
init config ports flags =
    let
        model : Model graph function
        model =
            { workers = WorkerQueue.init flags.workersCount
            , queue = Deque.empty
            , runStatus = LoadingParams
            }
    in
    Update.update model
        |> Update.addCmd
            (ports.toBackend
                { index = 0
                , data = Codec.encodeToValue (Types.toBackendCodec config) TBParams
                }
            )


view : Model graph function -> Element (Msg graph function)
view model =
    let
        workersLine : Element msg
        workersLine =
            text <| "Free workers: " ++ String.fromInt (WorkerQueue.freeCount model.workers)
    in
    column [ Theme.padding, Theme.spacing ] <|
        case model.runStatus of
            LoadingParams ->
                [ workersLine
                , text "Loading param list..."
                ]

            Ready inputs ->
                [ workersLine
                , text <| "Will run with " ++ String.fromInt (List.length inputs.params) ++ " params"
                , Theme.button []
                    { label = text "Start"
                    , onPress = Just (Start inputs)
                    }
                ]

            Running inputs results ->
                [ workersLine
                , text <| "Running... " ++ viewPercentage inputs.params results
                , Theme.button []
                    { label = text "Stop"
                    , onPress = Just (Stop inputs results)
                    }
                , viewResults results
                ]

            Finished inputs results ->
                [ workersLine
                , text "Done"
                , Theme.button []
                    { label = text "Restart"
                    , onPress = Just (Start inputs)
                    }
                , viewResults results
                ]

            Stopped inputs results ->
                [ workersLine
                , text <| "Stopped at " ++ viewPercentage inputs.params results
                , Theme.button []
                    { label = text "Restart"
                    , onPress = Just (Start inputs)
                    }
                , viewResults results
                ]


viewPercentage : List (Param graph function) -> Results -> String
viewPercentage params results =
    let
        paramCount : Int
        paramCount =
            List.length params

        rc : Int
        rc =
            resultsCount results

        percentage : Int
        percentage =
            (rc * 100) // paramCount
    in
    String.fromInt rc
        ++ "/"
        ++ String.fromInt paramCount
        ++ " ("
        ++ String.fromInt percentage
        ++ "%)"


resultsCount : Results -> Int
resultsCount results =
    Dict.foldl
        (\_ graph acc ->
            Dict.foldl
                (\_ function i_acc ->
                    Dict.size function + i_acc
                )
                acc
                graph
        )
        0
        results


viewResults : Results -> Element (Msg graph function)
viewResults results =
    let
        colorCount : Int
        colorCount =
            results
                |> Dict.foldl (\_ graph -> max (Dict.size graph)) 0

        colors : List Color.Color
        colors =
            List.range 0 (colorCount - 1)
                |> List.map
                    (\i ->
                        let
                            ( r, g, b ) =
                                Frontend.OkLch.oklchToSRGB
                                    ( 0.75
                                    , 0.126
                                    , toFloat i * 360 / toFloat colorCount
                                    )
                        in
                        Color.rgb r g b
                    )
    in
    results
        |> Dict.toList
        |> List.map
            (\( graphName, graph ) ->
                column [ Theme.spacing ]
                    [ text graphName
                    , graph
                        |> Dict.toList
                        |> List.map2
                            (\color ( _, data ) ->
                                ( color, data )
                            )
                            colors
                        |> Frontend.LinePlot.view
                        |> Element.html
                        |> el []
                    , graph
                        |> Dict.toList
                        |> List.map2
                            (\color ( functionName, _ ) ->
                                let
                                    rgba : { red : Float, green : Float, blue : Float, alpha : Float }
                                    rgba =
                                        Color.toRgba color
                                in
                                row []
                                    [ el
                                        [ Background.color <|
                                            Element.rgb rgba.red rgba.green rgba.blue
                                        , width <| px 10
                                        , height <| px 10
                                        , centerY
                                        ]
                                        Element.none
                                    , text " "
                                    , text functionName
                                    ]
                            )
                            colors
                        |> column []
                    ]
            )
        |> wrappedRow [ Theme.spacing ]


update : Config graph function -> Ports (Msg graph function) -> Msg graph function -> Model graph function -> ( Model graph function, Cmd (Msg graph function) )
update config ports msg model =
    let
        ( newModel, cmd ) =
            case msg of
                Start inputs ->
                    Update.update
                        (List.foldl sendToBackend
                            { model | runStatus = Running inputs Dict.empty }
                            inputs.params
                        )

                Stop params results ->
                    ( { model
                        | runStatus = Stopped params results
                        , workers = WorkerQueue.init (WorkerQueue.totalSize model.workers)
                        , queue = Deque.empty
                      }
                    , ports.terminateAll {}
                    )

                FromBackend index fb ->
                    let
                        freed : Model graph function
                        freed =
                            { model
                                | workers = WorkerQueue.addFree index model.workers
                            }
                    in
                    case fb of
                        TFResult param (Err _) ->
                            Update.update freed
                                |> Update.map (removeBigger param)

                        TFResult param (Ok stats) ->
                            case freed.runStatus of
                                Running inputs results ->
                                    let
                                        oldResults : Dict GraphName (Dict FunctionName (Dict Int Stats))
                                        oldResults =
                                            results

                                        newResults : Dict GraphName (Dict FunctionName (Dict Int Stats))
                                        newResults =
                                            upsert
                                                (config.graphToString param.graph)
                                                (\graphDict ->
                                                    upsert_
                                                        (config.functionToString param.function)
                                                        (Dict.insert param.size stats)
                                                        graphDict
                                                )
                                                oldResults
                                    in
                                    Update.update freed
                                        |> Update.map
                                            (if stats.median < Maybe.withDefault stats.median inputs.timeout then
                                                identity

                                             else
                                                removeBigger param
                                            )
                                        |> Update.map
                                            (\removed ->
                                                { removed
                                                    | runStatus =
                                                        if Deque.isEmpty removed.queue && WorkerQueue.areAllFree removed.workers then
                                                            Finished inputs newResults

                                                        else
                                                            Running inputs newResults
                                                }
                                            )

                                _ ->
                                    Update.update freed

                        TFParams params ->
                            Update.update
                                { freed | runStatus = Ready params }

                Nop ->
                    Update.update model
    in
    ( newModel, cmd )
        |> Update.andThen (trySend config ports)


removeBigger : Param graph function -> Model graph function -> Model graph function
removeBigger param model =
    let
        filter : Param graph function -> Bool
        filter p =
            (p.graph /= param.graph)
                || (p.function /= param.function)
                || (p.size < param.size)
    in
    { model
        | queue =
            Deque.filter filter
                model.queue
        , runStatus =
            case model.runStatus of
                LoadingParams ->
                    LoadingParams

                Ready inputs ->
                    Ready { inputs | params = List.filter filter inputs.params }

                Running inputs results ->
                    Running { inputs | params = List.filter filter inputs.params } results

                Finished inputs results ->
                    Finished { inputs | params = List.filter filter inputs.params } results

                Stopped inputs results ->
                    Stopped { inputs | params = List.filter filter inputs.params } results
    }


upsert : comparable -> (Dict comparable2 v -> Dict comparable2 v) -> Dict comparable (Dict comparable2 v) -> Dict comparable (Dict comparable2 v)
upsert key f =
    Dict.update key (Maybe.withDefault Dict.empty >> f >> Just)


upsert_ : comparable -> (Dict comparable2 v -> Dict comparable2 v) -> Dict comparable (Dict comparable2 v) -> Dict comparable (Dict comparable2 v)
upsert_ key f =
    Dict.update key (Maybe.withDefault Dict.empty >> f >> Just)


subscriptions : Config graph function -> Ports (Msg graph function) -> Sub (Msg graph function)
subscriptions config ports =
    receiveFromBackend config ports



-- PORTS --


sendToBackend : Param graph function -> Model graph function -> Model graph function
sendToBackend tb model =
    { model | queue = Deque.pushBack tb model.queue }


trySend : Config graph function -> Ports (Msg graph function) -> Model graph function -> ( Model graph function, Cmd (Msg graph function) )
trySend config ports model =
    let
        ( freeWorker, workers ) =
            WorkerQueue.getOne model.workers

        ( toSend, queue ) =
            Deque.popFront model.queue
    in
    case ( freeWorker, toSend ) of
        ( Just index, Just param ) ->
            ( { model | workers = workers, queue = queue }
            , ports.toBackend
                { index = index
                , data = Codec.encodeToValue (Types.toBackendCodec config) (TBRun param)
                }
            )
                |> Update.andThen (trySend config ports)

        _ ->
            ( model, Cmd.none )


receiveFromBackend : Config graph function -> Ports (Msg graph function) -> Sub (Msg graph function)
receiveFromBackend config ports =
    let
        codec : Codec (ToFrontend graph function)
        codec =
            Types.toFrontendCodec config
    in
    ports.fromBackend
        (\{ index, data } ->
            case Codec.decodeValue codec data of
                Ok decoded ->
                    FromBackend index decoded

                Err _ ->
                    Nop
        )