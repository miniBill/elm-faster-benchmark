module ToBenchmark exposing (Function, Graph, config)

import Codec exposing (Codec)
import Common.Types exposing (Config)
import Dict exposing (Dict)


config : Config Graph Function
config =
    { graphToString = graphToString
    , graphCodec = graphCodec
    , functionToString = functionToString
    , functionCodec = functionCodec
    , graphs = graphs
    , functions = functions
    , sizes = sizes
    , toFunction = toFunction
    , timeout = timeout
    }


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
timeout : Maybe number
timeout =
    Just 3


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
fibSlow =
    let
        go : Dict Int Int -> Int -> ( Dict Int Int, Int )
        go acc n =
            if n < 2 then
                ( acc, 1 )

            else
                case Dict.get n acc of
                    Nothing ->
                        let
                            ( acc2, m1 ) =
                                go acc (n - 1)

                            ( acc3, m2 ) =
                                go acc2 (n - 2)

                            res : Int
                            res =
                                m1 + m2
                        in
                        ( Dict.insert n res acc3, res )

                    Just cached ->
                        ( acc, cached )
    in
    go Dict.empty >> Tuple.second


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
