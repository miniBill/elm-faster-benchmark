module Types exposing (Index, Param, ToBackend(..), ToFrontend(..), toBackendCodec, toFrontendCodec)

import Benchmark.Parametric exposing (Stats)
import Codec exposing (Codec)
import ToBenchmark exposing (Function, Graph)


type ToFrontend
    = TFParams { timeout : Float, params : List Param }
    | TFResult Param (Result String Stats)


toFrontendCodec : Codec ToFrontend
toFrontendCodec =
    Codec.custom
        (\fparams fresult value ->
            case value of
                TFParams i ->
                    fparams i.timeout i.params

                TFResult param stats ->
                    fresult param stats
        )
        |> Codec.variant2 "TFParams"
            (\timeout params ->
                TFParams
                    { timeout = timeout
                    , params = params
                    }
            )
            Codec.float
            (Codec.list paramCodec)
        |> Codec.variant2 "TFResult"
            TFResult
            paramCodec
            (Codec.result Codec.string Benchmark.Parametric.statsCodec)
        |> Codec.buildCustom


type ToBackend
    = TBParams
    | TBRun Param


toBackendCodec : Codec ToBackend
toBackendCodec =
    Codec.custom
        (\fparams frun value ->
            case value of
                TBParams ->
                    fparams

                TBRun param ->
                    frun param
        )
        |> Codec.variant0 "TBParams" TBParams
        |> Codec.variant1 "TBRun" TBRun paramCodec
        |> Codec.buildCustom


type alias Index =
    Int


type alias Param =
    { graph : Graph
    , function : Function
    , size : Int
    }


paramCodec : Codec Param
paramCodec =
    Codec.object
        (\graph function size ->
            { graph = graph
            , function = function
            , size = size
            }
        )
        |> Codec.field "graph" .graph ToBenchmark.graphCodec
        |> Codec.field "function" .function ToBenchmark.functionCodec
        |> Codec.field "size" .size Codec.int
        |> Codec.buildObject
