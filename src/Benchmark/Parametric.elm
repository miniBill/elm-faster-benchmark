module Benchmark.Parametric exposing
    ( run
    , Stats, computeStatistics
    , statsCodec
    )

{-|


# Run tests

@docs run


# Calculate statistics

@docs Stats, computeStatistics


# Codec

@docs statsCodec

-}

import Benchmark.LowLevel exposing (Operation)
import Codec exposing (Codec)
import Statistics
import Task exposing (Task)


{-| Runs a given `Operation` and returns up to 100 timings (in ms).
-}
run : Operation -> Task String (List Float)
run operation =
    Benchmark.LowLevel.warmup operation
        |> Task.andThen (\_ -> Benchmark.LowLevel.findSampleSize operation)
        |> Task.andThen
            (\sampleSize ->
                let
                    batchCount : Int
                    batchCount =
                        min sampleSize 100

                    batchSize : Int
                    batchSize =
                        (sampleSize + 99) // 100
                in
                List.range 0 batchCount
                    |> List.map (\_ -> Benchmark.LowLevel.sample batchSize operation)
                    |> Task.sequence
                    |> Task.map (List.map (\totalDuration -> totalDuration / toFloat batchSize))
            )
        |> Task.mapError
            (\e ->
                case e of
                    Benchmark.LowLevel.StackOverflow ->
                        "Stack overflow"

                    Benchmark.LowLevel.UnknownError msg ->
                        msg
            )


type alias Stats =
    { firstQuartile : Float
    , median : Float
    , thirdQuartile : Float
    , max : Float
    , min : Float
    , outliers : List Float
    }


computeStatistics : List Float -> Stats
computeStatistics yList =
    let
        sortedYList : List Float
        sortedYList =
            List.sort yList

        -- Gather stats
        firstQuartile : Float
        firstQuartile =
            Statistics.quantile 0.25 sortedYList
                |> Maybe.withDefault 0

        median : Float
        median =
            Statistics.quantile 0.5 sortedYList
                |> Maybe.withDefault 0

        thirdQuartile : Float
        thirdQuartile =
            Statistics.quantile 0.75 sortedYList
                |> Maybe.withDefault 0

        interQuartileRange : Float
        interQuartileRange =
            thirdQuartile - firstQuartile

        whiskerTopMax : Float
        whiskerTopMax =
            thirdQuartile + 1.5 * interQuartileRange

        whiskerBottomMin : Float
        whiskerBottomMin =
            firstQuartile - (1.5 * interQuartileRange)

        ( outliersUnder, min, midAndOutliersOver ) =
            splitWhen (\n -> n >= whiskerBottomMin) sortedYList

        ( max, outliersOver ) =
            findLastSuchThat (\n -> n <= whiskerTopMax) midAndOutliersOver
    in
    { firstQuartile = firstQuartile
    , median = median
    , thirdQuartile = thirdQuartile
    , min = Maybe.withDefault 0 min
    , max = Maybe.withDefault 0 max
    , outliers = outliersUnder ++ outliersOver
    }


{-| The `Maybe a` is the first element which makes the function true
-}
splitWhen : (a -> Bool) -> List a -> ( List a, Maybe a, List a )
splitWhen f orig =
    let
        go : List a -> List a -> ( List a, Maybe a, List a )
        go acc input =
            case input of
                [] ->
                    ( orig, Nothing, [] )

                h :: t ->
                    if f h then
                        ( List.reverse acc, Just h, t )

                    else
                        go (h :: acc) t
    in
    go [] orig


{-| The `Maybe a` is the last element which makes the function true
-}
findLastSuchThat : (a -> Bool) -> List a -> ( Maybe a, List a )
findLastSuchThat f orig =
    let
        go : Maybe a -> List a -> ( Maybe a, List a )
        go eacc input =
            case input of
                [] ->
                    ( eacc, [] )

                h :: t ->
                    if f h then
                        go (Just h) t

                    else
                        ( Just h, t )
    in
    go Nothing orig


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
