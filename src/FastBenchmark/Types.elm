module FastBenchmark.Types exposing
    ( Config, Stats, Param
    , ToFrontend(..), ToBackend(..), Index
    , toFrontendCodec, toBackendCodec
    )

{-|


# Types

@docs Config, Stats, Param


# Messages

@docs ToFrontend, ToBackend, Index


# Codecs

@docs toFrontendCodec, toBackendCodec

-}

import Codec exposing (Codec)


{-| All the information needed to run a benchmark.
-}
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


{-| Statistics un run times.
-}
type alias Stats =
    { firstQuartile : Float
    , median : Float
    , thirdQuartile : Float
    , max : Float
    , min : Float
    , outliers : List Float
    }


{-| Message that gets sent to the frontend.
-}
type ToFrontend graph function
    = TFParams { timeout : Maybe Float, params : List (Param graph function) }
    | TFResult (Param graph function) (Result String Stats)


{-| Message that gets sent to the backend.
-}
type ToBackend graph function
    = TBParams
    | TBRun (Param graph function)


{-| Index of the Web Worker.
-}
type alias Index =
    Int


{-| Parameters needed to measure the timing.
-}
type alias Param graph function =
    { graph : graph
    , function : function
    , size : Int
    }


{-| `Codec` used to send messages to the frontend.
-}
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
            (Codec.result Codec.string statsCodec)
        |> Codec.buildCustom


{-| `Codec` used to send messages to the backend.
-}
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
