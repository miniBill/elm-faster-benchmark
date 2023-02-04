module Common.Types exposing (Config, Index, Param, ToBackend(..), ToFrontend(..), toBackendCodec, toFrontendCodec)

import Backend.Benchmark exposing (Stats)
import Codec exposing (Codec)


type alias Config graph function =
    { graphToString : graph -> String
    , graphCodec : Codec graph
    , functionToString : function -> String
    , functionCodec : Codec function

    --
    , graphs : List graph
    , functions : List function
    , sizes : List Int
    , toFunction : Param graph function -> (() -> ())

    --
    , timeout : Maybe Float
    }


type ToFrontend graph function
    = TFParams { timeout : Maybe Float, params : List (Param graph function) }
    | TFResult (Param graph function) (Result String Stats)


toFrontendCodec : Config graph function -> Codec (ToFrontend graph function)
toFrontendCodec config =
    let
        paramCodec_ : Codec (Param graph function)
        paramCodec_ =
            paramCodec config
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
            (Codec.result Codec.string Backend.Benchmark.statsCodec)
        |> Codec.buildCustom


type ToBackend graph function
    = TBParams
    | TBRun (Param graph function)


toBackendCodec : Config graph function -> Codec (ToBackend graph function)
toBackendCodec config =
    Codec.custom
        (\fparams frun value ->
            case value of
                TBParams ->
                    fparams

                TBRun param ->
                    frun param
        )
        |> Codec.variant0 "TBParams" TBParams
        |> Codec.variant1 "TBRun" TBRun (paramCodec config)
        |> Codec.buildCustom


type alias Index =
    Int


type alias Param graph function =
    { graph : graph
    , function : function
    , size : Int
    }


paramCodec : Config graph function -> Codec (Param graph function)
paramCodec { graphCodec, functionCodec } =
    Codec.object
        (\graph function size ->
            { graph = graph
            , function = function
            , size = size
            }
        )
        |> Codec.field "graph" .graph graphCodec
        |> Codec.field "function" .function functionCodec
        |> Codec.field "size" .size Codec.int
        |> Codec.buildObject
