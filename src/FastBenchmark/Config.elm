module FastBenchmark.Config exposing
    ( Config
    , init, withTimeout
    , params
    , functionToString, graphTitle
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

@docs functionToString, graphTitle


# Other configuration

@docs timeout, runFunction


# Codecs

@docs functionCodec, graphCodec

-}

import Benchmark.LowLevel exposing (Operation)
import Codec exposing (Codec)
import FastBenchmark.Types exposing (Param)
import List.Extra


{-| All the information needed to run a benchmark.
-}
type Config graph function
    = Config
        { graphTitle : graph -> String
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


{-| Initializes the benchmark configuration with some mandatory options.
-}
init :
    { graphTitle : graph -> String
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
        { graphTitle = config.graphTitle
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


{-| Sets a timeout (in ms) for each `Param`.
-}
withTimeout : Float -> Config graph function -> Config graph function
withTimeout newTimeout (Config config) =
    Config { config | timeout = Just newTimeout }


{-| Codec for graphs.
-}
graphCodec : Config graph function -> Codec graph
graphCodec (Config config) =
    config.graphCodec


{-| Codec for functions.
-}
functionCodec : Config graph function -> Codec function
functionCodec (Config config) =
    config.functionCodec


{-| Gets a graph title.
-}
graphTitle : Config graph function -> graph -> String
graphTitle (Config config) =
    config.graphTitle


{-| Converts a function to a user-visible string.
-}
functionToString : Config graph function -> function -> String
functionToString (Config config) =
    config.functionToString


{-| Gets the list of all possible params to benchmark.
-}
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


{-| Gets the timeout (in ms) for each `Param`.
-}
timeout : Config graph function -> Maybe Float
timeout (Config config) =
    config.timeout


{-| Prepares a function for running inside a benchmark
-}
runFunction : Config graph function -> Param graph function -> Operation
runFunction (Config config) param =
    Benchmark.LowLevel.operation (config.runFunction param)
