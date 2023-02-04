module ToBenchmark exposing (Function(..), Graph(..), functionCodec, functionToString, functions, graphCodec, graphToString, graphs, sizes, timeout, toFunction)

import Codec exposing (Codec)


type Graph
    = Simple


graphToString : Graph -> String
graphToString graph =
    case graph of
        Simple ->
            "Comparing implementations"


graphCodec : Codec Graph
graphCodec =
    Codec.constant Simple


type Function
    = SlowFibonacci
    | FastFibonacci


functionToString : Function -> String
functionToString function =
    case function of
        SlowFibonacci ->
            "Slow"

        FastFibonacci ->
            "Fast"


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


{-| Timeout, in milliseconds
-}
timeout : number
timeout =
    3


graphs : List Graph
graphs =
    [ Simple ]


functions : List Function
functions =
    [ FastFibonacci
    , SlowFibonacci
    ]


sizes : List Int
sizes =
    List.range 1 100


toFunction : Function -> (Int -> ())
toFunction function =
    case function of
        SlowFibonacci ->
            ignore << fibSlow

        FastFibonacci ->
            ignore << fibFast


fibSlow : Int -> Int
fibSlow n =
    if n < 2 then
        1

    else
        fibSlow (n - 1) + fibSlow (n - 2)


fibFast : Int -> Int
fibFast n =
    List.range 2 n
        |> List.foldl
            (\_ ( high, low ) -> ( high + low, high ))
            ( 1, 1 )
        |> Tuple.first


ignore : a -> ()
ignore _ =
    ()
