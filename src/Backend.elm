port module Backend exposing (Flags, Model, Msg, main)

import Benchmark.LowLevel
import Benchmark.Parametric
import Codec exposing (Value)
import List.Extra
import Task
import ToBenchmark
import Types exposing (Param, ToBackend(..), ToFrontend(..))


params : List Param
params =
    List.Extra.lift3
        (\graph size function ->
            { graph = graph
            , function = function
            , size = size
            }
        )
        ToBenchmark.graphs
        ToBenchmark.sizes
        ToBenchmark.functions


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
            sendToFrontend <|
                TFParams
                    { timeout = ToBenchmark.timeout
                    , params = params
                    }

        Nop ->
            Cmd.none

        FromFrontend (TBRun param) ->
            let
                function : Int -> ()
                function =
                    ToBenchmark.toFunction param.function

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
    toFrontend (Codec.encodeToValue Types.toFrontendCodec tf)


receiveFromFrontend : Sub Msg
receiveFromFrontend =
    fromFrontend
        (\value ->
            case Codec.decodeValue Types.toBackendCodec value of
                Ok decoded ->
                    FromFrontend decoded

                Err _ ->
                    Nop
        )
