module Codecs exposing (paramCodec, toBackendCodec, toFrontendCodec)

import Benchmark.Parametric
import Codec exposing (Codec)
import Types exposing (Function(..), Param, ToBackend(..), ToFrontend(..))


toFrontendCodec : Codec ToFrontend
toFrontendCodec =
    Codec.custom
        (\fparams fresult value ->
            case value of
                TFParams i ->
                    fparams i

                TFResult param stats ->
                    fresult param stats
        )
        |> Codec.variant1 "TFParams" TFParams (Codec.list paramCodec)
        |> Codec.variant2 "TFResult"
            TFResult
            paramCodec
            (Codec.result Codec.string Benchmark.Parametric.statsCodec)
        |> Codec.buildCustom


toBackendCodec : Codec ToBackend
toBackendCodec =
    Codec.custom
        (\fparams fparam value ->
            case value of
                TBParams ->
                    fparams

                TBParam param ->
                    fparam param
        )
        |> Codec.variant0 "TBParams" TBParams
        |> Codec.variant1 "TBParam" TBParam paramCodec
        |> Codec.buildCustom


paramCodec : Codec Param
paramCodec =
    Codec.object
        (\graphName function size ->
            { graphName = graphName
            , function = function
            , size = size
            }
        )
        |> Codec.field "graph" .graphName Codec.string
        |> Codec.field "function" .function functionCodec
        |> Codec.field "size" .size Codec.int
        |> Codec.buildObject


functionCodec : Codec Function
functionCodec =
    Codec.custom
        (\fslow ffast value ->
            case value of
                SlowFibonacci ->
                    fslow

                FastFibonacci ->
                    ffast
        )
        |> Codec.variant0 "SlowFibonacci" SlowFibonacci
        |> Codec.variant0 "FastFibonacci" FastFibonacci
        |> Codec.buildCustom
