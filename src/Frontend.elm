port module Frontend exposing (Flags, Model, Msg, RunStatus, main)

import Benchmark.Parametric exposing (Stats)
import Browser
import Codec exposing (Value)
import Codecs
import Color exposing (Color)
import Deque exposing (Deque)
import Dict exposing (Dict)
import Element exposing (Element, centerY, column, el, height, px, row, text, width, wrappedRow)
import Element.Background as Background
import LinePlot
import Theme
import Types exposing (GraphName, Index, Param, ToBackend(..), ToFrontend(..))
import Update
import WorkerQueue exposing (WorkerQueue)


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


type alias FunctionName =
    String


type RunStatus
    = LoadingParams
    | Ready (List Param)
    | Running (List Param) Results
    | Finished (List Param) Results
    | Stopped (List Param) Results


type Msg
    = FromBackend Index ToFrontend
    | Start (List Param)
    | Stop (List Param) Results
    | Nop


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
                , data = Codec.encodeToValue Codecs.toBackendCodec TBParams
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

            Ready params ->
                [ workersLine
                , text <| "Will run with " ++ String.fromInt (List.length params) ++ " params"
                , Theme.button []
                    { label = text "Start"
                    , onPress = Just (Start params)
                    }
                ]

            Running params results ->
                [ workersLine
                , text <| "Running... " ++ viewPercentage params results
                , Theme.button []
                    { label = text "Stop"
                    , onPress = Just (Stop params results)
                    }
                , viewResults results
                ]

            Finished params results ->
                [ workersLine
                , text "Done"
                , Theme.button []
                    { label = text "Restart"
                    , onPress = Just (Start params)
                    }
                , viewResults results
                ]

            Stopped params results ->
                [ workersLine
                , text <| "Stopped at " ++ viewPercentage params results
                , Theme.button []
                    { label = text "Restart"
                    , onPress = Just (Start params)
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


colors : List Color
colors =
    [ Color.blue
    , Color.red
    ]


viewResults : Results -> Element Msg
viewResults results =
    results
        |> Dict.toList
        |> List.map
            (\( graphName, graph ) ->
                column [ Theme.spacing ]
                    [ text graphName
                    , graph
                        |> Dict.toList
                        |> List.map2 (\color ( _, data ) -> ( color, data )) colors
                        |> LinePlot.view
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


{-| Timeout, in milliseconds
-}
timeout : number
timeout =
    3


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( newModel, cmd ) =
            case msg of
                Start params ->
                    Update.update
                        (List.foldl sendToBackend
                            { model | runStatus = Running params Dict.empty }
                            params
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
                                Running params results ->
                                    let
                                        oldResults : Dict GraphName (Dict FunctionName (Dict Int Stats))
                                        oldResults =
                                            results

                                        newResults : Dict GraphName (Dict FunctionName (Dict Int Stats))
                                        newResults =
                                            upsert
                                                param.graphName
                                                tmp
                                                oldResults

                                        tmp : Dict FunctionName (Dict Int Stats) -> Dict FunctionName (Dict Int Stats)
                                        tmp graphDict =
                                            upsert_
                                                (Types.functionToString param.function)
                                                (Dict.insert param.size stats)
                                                graphDict
                                    in
                                    Update.update freed
                                        |> Update.map
                                            (if stats.median < timeout then
                                                identity

                                             else
                                                removeBigger param
                                            )
                                        |> Update.map
                                            (\removed ->
                                                { removed
                                                    | runStatus =
                                                        if Deque.isEmpty removed.queue && WorkerQueue.areAllFree removed.workers then
                                                            Finished params newResults

                                                        else
                                                            Running params newResults
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
            (p.graphName /= param.graphName)
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

                Ready params ->
                    Ready (List.filter filter params)

                Running params results ->
                    Running (List.filter filter params) results

                Finished params results ->
                    Finished (List.filter filter params) results

                Stopped params results ->
                    Stopped (List.filter filter params) results
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
                , data = Codec.encodeToValue Codecs.toBackendCodec (TBParam param)
                }
            )
                |> Update.andThen trySend

        _ ->
            ( model, Cmd.none )


receiveFromBackend : Sub Msg
receiveFromBackend =
    fromBackend
        (\{ index, data } ->
            case Codec.decodeValue Codecs.toFrontendCodec data of
                Ok decoded ->
                    FromBackend index decoded

                Err _ ->
                    Nop
        )
