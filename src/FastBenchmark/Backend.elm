module FastBenchmark.Backend exposing
    ( app
    , Msg, Ports, Program
    )

{-|


# Main application

@docs app


# Types

@docs Msg, Ports, Program

-}

import Benchmark.LowLevel
import Codec exposing (Value)
import FastBenchmark.Backend.Benchmark
import FastBenchmark.Codecs as Codecs
import FastBenchmark.Config as Config exposing (Config)
import FastBenchmark.Types exposing (Param, Stats, ToBackend(..), ToFrontend(..))
import Task


{-| Ports needed for the backend.
-}
type alias Ports msg =
    { fromFrontend : (Value -> msg) -> Sub msg
    , toFrontend : Value -> Cmd msg
    }


{-| The message type for the backend.
-}
type Msg graph function
    = FromFrontend (ToBackend graph function)
    | RunResult TryCount (Param graph function) (Result String (List Float))
    | Nop


type alias TryCount =
    Int


maxTries : TryCount
maxTries =
    10


{-| A convenient type for a backend `Program`.
-}
type alias Program graph function =
    Platform.Program {} {} (Msg graph function)


{-| Main backend app.
-}
app : Config graph function -> Ports (Msg graph function) -> Program graph function
app config ports =
    Platform.worker
        { init = \_ -> ( {}, Cmd.none )
        , update = \msg model -> ( model, toCmd config ports msg )
        , subscriptions =
            let
                subs : Sub (Msg graph function)
                subs =
                    subscriptions config ports
            in
            \_ -> subs
        }


toCmd : Config graph function -> Ports (Msg graph function) -> Msg graph function -> Cmd (Msg graph function)
toCmd config ports msg =
    case msg of
        FromFrontend TBParams ->
            sendToFrontend config ports <|
                TFParams
                    { timeout = Config.timeout config
                    , params = Config.params config
                    }

        Nop ->
            Cmd.none

        FromFrontend (TBRun param) ->
            doRun config 0 [] param

        RunResult try param (Ok res) ->
            let
                stats : Stats
                stats =
                    FastBenchmark.Backend.Benchmark.computeStatistics res
            in
            if try < maxTries && isTooWide stats then
                doRun config try res param

            else
                sendToFrontend config ports (TFResult param (Ok stats))

        RunResult _ param (Err e) ->
            sendToFrontend config ports (TFResult param (Err e))


isTooWide : Stats -> Bool
isTooWide stats =
    abs (stats.max - stats.min) > (stats.median * 0.1)


doRun : Config graph function -> TryCount -> List Float -> Param graph function -> Cmd (Msg graph function)
doRun config try existing param =
    let
        operation : Benchmark.LowLevel.Operation
        operation =
            Config.runFunction config param
    in
    FastBenchmark.Backend.Benchmark.run operation
        |> Task.attempt
            (\newResults ->
                RunResult (try + 1)
                    param
                    (Result.map ((++) existing) newResults)
            )


subscriptions : Config graph function -> Ports (Msg graph function) -> Sub (Msg graph function)
subscriptions config ports =
    receiveFromFrontend config ports



-- PORTS --


sendToFrontend : Config graph function -> Ports (Msg graph function) -> ToFrontend graph function -> Cmd (Msg graph function)
sendToFrontend config ports tf =
    ports.toFrontend (Codec.encodeToValue (Codecs.toFrontendCodec config) tf)


receiveFromFrontend : Config graph function -> Ports (Msg graph function) -> Sub (Msg graph function)
receiveFromFrontend config ports =
    let
        codec : Codec.Codec (ToBackend graph function)
        codec =
            Codecs.toBackendCodec config
    in
    ports.fromFrontend
        (\value ->
            case Codec.decodeValue codec value of
                Ok decoded ->
                    FromFrontend decoded

                Err _ ->
                    Nop
        )
