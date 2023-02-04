port module Ui.Frontend exposing (Flags, Model, Msg, RunStatus, main)

import Benchmark.Parametric exposing (Stats)
import Benchmark.WorkerQueue as WorkerQueue exposing (WorkerQueue)
import Browser
import Codec exposing (Value)
import Color
import Common.Types as Types exposing (Index, Param, ToBackend(..), ToFrontend(..))
import Deque exposing (Deque)
import Dict exposing (Dict)
import Element exposing (Element, centerY, column, el, height, px, row, text, width, wrappedRow)
import Element.Background as Background
import ToBenchmark
import Ui.LinePlot
import Ui.OkLch
import Ui.Theme as Theme
import Ui.Update as Update


type alias Flags =
    { workersCount : Int
    }


type alias Model =
    { workers : WorkerQueue
    , queue : Deque Param
    , runStatus : RunStatus
    }


type alias Results =
    Dict GraphName (Dict FunctionName (Dict Int Stats))


type alias GraphName =
    String


type alias FunctionName =
    String


type RunStatus
    = LoadingParams
    | Ready Inputs
    | Running Inputs Results
    | Finished Inputs Results
    | Stopped Inputs Results


type Msg
    = FromBackend Index ToFrontend
    | Start Inputs
    | Stop Inputs Results
    | Nop


type alias Inputs =
    { timeout : Float
    , params : List Param
    }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = \model -> Element.layout [] <| view model
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model : Model
        model =
            { workers = WorkerQueue.init flags.workersCount
            , queue = Deque.empty
            , runStatus = LoadingParams
            }
    in
    Update.update model
        |> Update.addCmd
            (toBackend
                { index = 0
                , data = Codec.encodeToValue Types.toBackendCodec TBParams
                }
            )


view : Model -> Element Msg
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


viewPercentage : List Param -> Results -> String
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


viewResults : Results -> Element Msg
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
                                Ui.OkLch.oklchToSRGB
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
                        |> Ui.LinePlot.view
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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
                    , terminateAll {}
                    )

                FromBackend index fb ->
                    let
                        freed : Model
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
                                                (ToBenchmark.graphToString param.graph)
                                                (\graphDict ->
                                                    upsert_
                                                        (ToBenchmark.functionToString param.function)
                                                        (Dict.insert param.size stats)
                                                        graphDict
                                                )
                                                oldResults
                                    in
                                    Update.update freed
                                        |> Update.map
                                            (if stats.median < inputs.timeout then
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
        |> Update.andThen trySend


removeBigger : Param -> Model -> Model
removeBigger param model =
    let
        filter : Param -> Bool
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveFromBackend



-- PORTS --


port terminateAll : {} -> Cmd msg


port fromBackend : ({ index : Int, data : Value } -> msg) -> Sub msg


port toBackend : { index : Int, data : Value } -> Cmd msg


sendToBackend : Param -> Model -> Model
sendToBackend tb model =
    { model | queue = Deque.pushBack tb model.queue }


trySend : Model -> ( Model, Cmd Msg )
trySend model =
    let
        ( freeWorker, workers ) =
            WorkerQueue.getOne model.workers

        ( toSend, queue ) =
            Deque.popFront model.queue
    in
    case ( freeWorker, toSend ) of
        ( Just index, Just param ) ->
            ( { model | workers = workers, queue = queue }
            , toBackend
                { index = index
                , data = Codec.encodeToValue Types.toBackendCodec (TBRun param)
                }
            )
                |> Update.andThen trySend

        _ ->
            ( model, Cmd.none )


receiveFromBackend : Sub Msg
receiveFromBackend =
    fromBackend
        (\{ index, data } ->
            case Codec.decodeValue Types.toFrontendCodec data of
                Ok decoded ->
                    FromBackend index decoded

                Err _ ->
                    Nop
        )
