module FastBenchmark.Codecs exposing (toBackendCodec, toFrontendCodec)

{-| `Codec` used to send messages to the frontend.
-}

import Codec exposing (Codec)
import FastBenchmark.Config as Config exposing (Config)
import FastBenchmark.Types exposing (Param, Stats, ToBackend(..), ToFrontend(..))


toFrontendCodec : Config graph function -> Codec (ToFrontend graph function)
toFrontendCodec cfg =
    let
        paramCodec_ : Codec (Param graph function)
        paramCodec_ =
            paramCodec cfg
    in
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
            (Codec.maybe Codec.float)
            (Codec.list paramCodec_)
        |> Codec.variant2 "TFResult"
            TFResult
            paramCodec_
            (Codec.result Codec.string statsCodec)
        |> Codec.buildCustom


{-| `Codec` used to send messages to the backend.
-}
toBackendCodec : Config graph function -> Codec (ToBackend graph function)
toBackendCodec cfg =
    Codec.custom
        (\fparams frun value ->
            case value of
                TBParams ->
                    fparams

                TBRun param ->
                    frun param
        )
        |> Codec.variant0 "TBParams" TBParams
        |> Codec.variant1 "TBRun" TBRun (paramCodec cfg)
        |> Codec.buildCustom


paramCodec : Config graph function -> Codec (Param graph function)
paramCodec config =
    Codec.object
        (\graph function size ->
            { graph = graph
            , function = function
            , size = size
            }
        )
        |> Codec.field "graph" .graph (Config.graphCodec config)
        |> Codec.field "function" .function (Config.functionCodec config)
        |> Codec.field "size" .size Codec.int
        |> Codec.buildObject


statsCodec : Codec Stats
statsCodec =
    Codec.object
        (\firstQuartile median thirdQuartile max min outliers ->
            { firstQuartile = firstQuartile
            , median = median
            , thirdQuartile = thirdQuartile
            , max = max
            , min = min
            , outliers = outliers
            }
        )
        |> Codec.field "firstQuartile" .firstQuartile Codec.float
        |> Codec.field "median" .median Codec.float
        |> Codec.field "thirdQuartile" .thirdQuartile Codec.float
        |> Codec.field "max" .max Codec.float
        |> Codec.field "min" .min Codec.float
        |> Codec.field "outliers" .outliers (Codec.list Codec.float)
        |> Codec.buildObject
