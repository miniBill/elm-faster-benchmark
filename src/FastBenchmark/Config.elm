module FastBenchmark.Config exposing
    ( Config
    , init, withTimeout
    , params
    , functionToString, graphToString
    , timeout, toFunction
    , functionCodec, graphCodec
    )

{-|


# Types

@docs Config


# Building configuration

@docs init, withTimeout


# Params

@docs params


# Visualization

@docs functionToString, graphToString


# Other configuration

@docs timeout, toFunction


# Codecs

@docs functionCodec, graphCodec

-}

import Codec exposing (Codec)
import FastBenchmark.Types exposing (Param)
import List.Extra


{-| All the information needed to run a benchmark.
-}
type Config graph function
    = Config
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


init :
    { graphToString : graph -> String
    , graphCodec : Codec graph
    , functionToString : function -> String
    , functionCodec : Codec function

    --
    , graphs : List graph
    , functions : List function
    , sizes : List Int
    , toFunction : Param graph function -> (() -> ())
    }
    -> Config graph function
init config =
    Config
        { graphToString = config.graphToString
        , graphCodec = config.graphCodec
        , functionToString = config.functionToString
        , functionCodec = config.functionCodec

        --
        , graphs = config.graphs
        , functions = config.functions
        , sizes = config.sizes
        , toFunction = config.toFunction

        --
        , timeout = Nothing
        }


withTimeout : Float -> Config graph function -> Config graph function
withTimeout newTimeout (Config config) =
    Config { config | timeout = Just newTimeout }


graphCodec : Config graph function -> Codec graph
graphCodec (Config config) =
    config.graphCodec


functionCodec : Config graph function -> Codec function
functionCodec (Config config) =
    config.functionCodec


graphToString : Config graph function -> graph -> String
graphToString (Config config) =
    config.graphToString


functionToString : Config graph function -> function -> String
functionToString (Config config) =
    config.functionToString


params : Config graph function -> List (Param graph function)
params (Config config) =
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


timeout : Config graph function -> Maybe Float
timeout (Config config) =
    config.timeout


toFunction : Config graph function -> Param graph function -> () -> ()
toFunction (Config config) =
    config.toFunction
