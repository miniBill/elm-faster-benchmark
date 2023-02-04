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


type alias Flags =
    { workersCount : Int
    }


type alias Model =
    { workersCount : Int
    , freeWorkers : Deque Index
    , queue : Deque ToBackend
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
            { workersCount = flags.workersCount
            , freeWorkers = allFree flags.workersCount
            , queue = Deque.empty
            , runStatus = LoadingParams
            }
    in
    Update.update model
        |> Update.map (sendToBackend TBParams)
        |> Update.andThen trySend


allFree : Int -> Deque Int
allFree workersCount =
    Deque.fromList (List.range 0 (workersCount - 1))


view : Model -> Element Msg
view model =
    let
        workersLine : Element msg
        workersLine =
            text <| "Free workers: " ++ String.fromInt (Deque.length model.freeWorkers)
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

        percentage : Int
        percentage =
            (resultsCount results * 100) // paramCount
    in
    String.fromInt percentage ++ "%"


resultsCount : Results -> Int
resultsCount results =
    Dict.foldl
        (\_ graph acc ->
            Dict.foldl
                (\_ function ->
                    (+) (Dict.size function)
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( newModel, cmd ) =
            case msg of
                Start params ->
                    Update.update
                        (List.foldl (\param -> sendToBackend (TBParam param))
                            { model | runStatus = Running params Dict.empty }
                            params
                        )

                Stop params results ->
                    ( { model
                        | runStatus = Stopped params results
                        , freeWorkers = allFree model.workersCount
                        , queue = Deque.empty
                      }
                    , terminateAll {}
                    )

                FromBackend index fb ->
                    let
                        freed : Model
                        freed =
                            { model
                                | freeWorkers = Deque.pushFront index model.freeWorkers
                            }
                    in
                    case fb of
                        TFResult _ (Err _) ->
                            -- TODO: handle error
                            Update.update freed

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
                                    Update.update
                                        { freed
                                            | runStatus =
                                                Running params newResults
                                        }

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


sendToBackend : ToBackend -> Model -> Model
sendToBackend tb model =
    { model | queue = Deque.pushBack tb model.queue }


trySend : Model -> ( Model, Cmd Msg )
trySend model =
    let
        ( freeWorker, freeWorkers ) =
            Deque.popFront model.freeWorkers

        ( toSend, queue ) =
            Deque.popFront model.queue
    in
    case ( freeWorker, toSend ) of
        ( Just index, Just tb ) ->
            ( { model | freeWorkers = freeWorkers, queue = queue }
            , toBackend
                { index = index
                , data = Codec.encodeToValue Codecs.toBackendCodec tb
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
