module FastBenchmark.Config exposing
    ( Config
    , init, withTimeout
    , params
    , functionToString, graphToString
    , timeout, runFunction
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

@docs timeout, runFunction


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
        , functionToString : function -> String

        --
        , graphCodec : Codec graph
        , functionCodec : Codec function

        --
        , graphs : List graph
        , graphData :
            graph
            ->
                { functions : List function
                , sizes : List Int
                }
        , runFunction : Param graph function -> (() -> ())

        --
        , timeout : Maybe Float
        }


init :
    { graphToString : graph -> String
    , functionToString : function -> String

    --
    , graphCodec : Codec graph
    , functionCodec : Codec function

    --
    , graphs : List graph
    , graphData :
        graph
        ->
            { functions : List function
            , sizes : List Int
            }
    , runFunction : Param graph function -> (() -> ())
    }
    -> Config graph function
init config =
    Config
        { graphToString = config.graphToString
        , functionToString = config.functionToString

        --
        , graphCodec = config.graphCodec
        , functionCodec = config.functionCodec

        --
        , graphs = config.graphs
        , graphData = config.graphData
        , runFunction = config.runFunction

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
    config.graphs
        |> List.concatMap
            (\graph ->
                let
                    { functions, sizes } =
                        config.graphData graph
                in
                List.Extra.lift2
                    (\function size ->
                        { graph = graph
                        , function = function
                        , size = size
                        }
                    )
                    functions
                    sizes
            )


timeout : Config graph function -> Maybe Float
timeout (Config config) =
    config.timeout


runFunction : Config graph function -> Param graph function -> () -> ()
runFunction (Config config) =
    config.runFunction
