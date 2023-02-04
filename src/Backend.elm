port module Backend exposing (Flags, Model, Msg, main)

import Benchmark.LowLevel
import Benchmark.Parametric
import Codec exposing (Value)
import Codecs
import Task
import Types exposing (Function(..), GraphName, Param, ToBackend(..), ToFrontend(..))


params : List Param
params =
    let
        graphNames : List GraphName
        graphNames =
            [ "Simple graph" ]

        functions : List Function
        functions =
            [ FastFibonacci

            -- , SlowFibonacci
            ]

        sizes : List Int
        sizes =
            List.range 1 100
    in
    graphNames
        |> List.concatMap
            (\graphName ->
                sizes
                    |> List.concatMap
                        (\size ->
                            functions
                                |> List.map
                                    (\function ->
                                        { graphName = graphName
                                        , function = function
                                        , size = size
                                        }
                                    )
                        )
            )


toFunction : Function -> (Int -> ())
toFunction function =
    case function of
        SlowFibonacci ->
            ignore << fibSlow

        FastFibonacci ->
            ignore << fibFast


ignore : a -> ()
ignore _ =
    ()


fibSlow : Int -> Int
fibSlow n =
    if n < 2 then
        1

    else
        fibSlow (n - 1) + fibSlow (n - 2)


fibFast : Int -> Int
fibFast n =
    List.range 2 n
        |> List.foldl
            (\_ ( high, low ) -> ( high + low, high ))
            ( 1, 1 )
        |> Tuple.first


type alias Flags =
    {}


type alias Model =
    {}


type Msg
    = FromFrontend ToBackend
    | RunResult Param (Result String (List Float))
    | Nop


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = \msg model -> ( model, toCmd msg )
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )


toCmd : Msg -> Cmd Msg
toCmd msg =
    case msg of
        FromFrontend TBParams ->
            sendToFrontend <| TFParams params

        Nop ->
            Cmd.none

        FromFrontend (TBParam param) ->
            let
                function : Int -> ()
                function =
                    toFunction param.function

                size : Int
                size =
                    param.size

                operation : Benchmark.LowLevel.Operation
                operation =
                    Benchmark.LowLevel.operation (\_ -> function size)
            in
            Benchmark.Parametric.run operation
                |> Task.attempt (RunResult param)

        RunResult param (Ok res) ->
            let
                stats : Benchmark.Parametric.Stats
                stats =
                    Benchmark.Parametric.computeStatistics res
            in
            sendToFrontend (TFResult param (Ok stats))

        RunResult param (Err e) ->
            sendToFrontend (TFResult param (Err e))


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveFromFrontend



-- PORTS --


port fromFrontend : (Value -> msg) -> Sub msg


port toFrontend : Value -> Cmd msg


sendToFrontend : ToFrontend -> Cmd msg
sendToFrontend tf =
    toFrontend (Codec.encodeToValue Codecs.toFrontendCodec tf)


receiveFromFrontend : Sub Msg
receiveFromFrontend =
    fromFrontend
        (\value ->
            case Codec.decodeValue Codecs.toBackendCodec value of
                Ok decoded ->
                    FromFrontend decoded

                Err _ ->
                    Nop
        )
