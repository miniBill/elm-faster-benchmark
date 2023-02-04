module Backend.App exposing (Flags, Model, Msg, Ports, Program, app)

import Backend.Benchmark
import Benchmark.LowLevel
import Codec exposing (Value)
import Common.Types as Types exposing (Config, Param, ToBackend(..), ToFrontend(..))
import List.Extra
import Task


type alias Ports msg =
    { fromFrontend : (Value -> msg) -> Sub msg
    , toFrontend : Value -> Cmd msg
    }


params : Config graph function -> List (Param graph function)
params config =
    List.Extra.lift3
        (\size graph function ->
            { size = size
            , graph = graph
            , function = function
            }
        )
        config.sizes
        config.graphs
        config.functions


type alias Flags =
    {}


type alias Model =
    {}


type Msg graph function
    = FromFrontend (ToBackend graph function)
    | RunResult (Param graph function) (Result String (List Float))
    | Nop


type alias Program graph function =
    Platform.Program Flags Model (Msg graph function)


app : Config graph function -> Ports (Msg graph function) -> Program graph function
app config ports =
    Platform.worker
        { init = init
        , update = \msg model -> ( model, toCmd config ports msg )
        , subscriptions =
            let
                subs : Sub (Msg graph function)
                subs =
                    subscriptions config ports
            in
            \_ -> subs
        }


init : Flags -> ( Model, Cmd msg )
init _ =
    ( {}, Cmd.none )


toCmd : Config graph function -> Ports (Msg graph function) -> Msg graph function -> Cmd (Msg graph function)
toCmd config ports msg =
    case msg of
        FromFrontend TBParams ->
            sendToFrontend config ports <|
                TFParams
                    { timeout = config.timeout
                    , params = params config
                    }

        Nop ->
            Cmd.none

        FromFrontend (TBRun param) ->
            let
                function : () -> ()
                function =
                    config.toFunction param

                operation : Benchmark.LowLevel.Operation
                operation =
                    Benchmark.LowLevel.operation function
            in
            Backend.Benchmark.run operation
                |> Task.attempt (RunResult param)

        RunResult param (Ok res) ->
            let
                stats : Backend.Benchmark.Stats
                stats =
                    Backend.Benchmark.computeStatistics res
            in
            sendToFrontend config ports (TFResult param (Ok stats))

        RunResult param (Err e) ->
            sendToFrontend config ports (TFResult param (Err e))


subscriptions : Config graph function -> Ports (Msg graph function) -> Sub (Msg graph function)
subscriptions config ports =
    receiveFromFrontend config ports



-- PORTS --


sendToFrontend : Config graph function -> Ports (Msg graph function) -> ToFrontend graph function -> Cmd (Msg graph function)
sendToFrontend config ports tf =
    ports.toFrontend (Codec.encodeToValue (Types.toFrontendCodec config) tf)


receiveFromFrontend : Config graph function -> Ports (Msg graph function) -> Sub (Msg graph function)
receiveFromFrontend config ports =
    let
        codec : Codec.Codec (ToBackend graph function)
        codec =
            Types.toBackendCodec config
    in
    ports.fromFrontend
        (\value ->
            case Codec.decodeValue codec value of
                Ok decoded ->
                    FromFrontend decoded

                Err _ ->
                    Nop
        )
