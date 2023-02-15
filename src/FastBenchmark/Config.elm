module FastBenchmark.Config exposing
    ( Config
    , init, withTimeout, withRetry
    , params
    , functionToString, graphTitle
    , timeout, retry, runFunction
    , functionCodec, graphCodec
    )

{-|


# Types

@docs Config


# Building configuration

@docs init, withTimeout, withRetry


# To params list

@docs params


# Visualization

@docs functionToString, graphTitle


# Other configuration

@docs timeout, retry, runFunction


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
        , retry : Maybe { times : Int, percentage : Float }
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
        , retry = Nothing
        }


{-| Sets a timeout (in ms) for each `Param`.
-}
withTimeout : Float -> Config graph function -> Config graph function
withTimeout newTimeout (Config config) =
    Config { config | timeout = Just newTimeout }


{-| Try running the benchmark more times (up to `times`) if `max - min` would be more than `percentage * median`.

Reasonable values are `5` for `times` and `0.5` for `percentage`.

-}
withRetry : { times : Int, percentage : Float } -> Config graph function -> Config graph function
withRetry r (Config config) =
    Config { config | retry = Just r }


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


{-| Gets the retry configuration.
-}
retry : Config graph function -> Maybe { times : Int, percentage : Float }
retry (Config config) =
    config.retry


{-| Prepares a function for running inside a benchmark
-}
runFunction : Config graph function -> Param graph function -> Operation
runFunction (Config config) param =
    Benchmark.LowLevel.operation (config.runFunction param)
