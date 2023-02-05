module ToBenchmark exposing (Function, Graph, config)

import Codec exposing (Codec)
import Dict exposing (Dict)
import FastBenchmark.Types exposing (Config, Param)


config : Config Graph Function
config =
    { graphToString = graphToString
    , graphCodec = graphCodec
    , functionToString = functionToString
    , functionCodec = functionCodec

    --
    , graphs = graphs
    , functions = functions
    , sizes = sizes
    , toFunction = toFunction

    --
    , timeout = timeout
    }


graphToString : Graph -> String
graphToString graph =
    case graph of
        Simple ->
            "Simple comparison"


graphCodec : Codec Graph
graphCodec =
    Codec.custom
        (\fsimple value ->
            case value of
                Simple ->
                    fsimple
        )
        |> Codec.variant0 "Simple" Simple
        |> Codec.buildCustom


functionToString : Function -> String
functionToString function =
    case function of
        Slow ->
            "Slow"

        Fast ->
            "Fast"


functionCodec : Codec Function
functionCodec =
    Codec.custom
        (\fslow ffast value ->
            case value of
                Slow ->
                    fslow

                Fast ->
                    ffast
        )
        |> Codec.variant0 "Slow" Slow
        |> Codec.variant0 "Fast" Fast
        |> Codec.buildCustom


graphs : List Graph
graphs =
    [ Simple ]


functions : List Function
functions =
    [ Slow
    , Fast
    ]


sizes : List Int
sizes =
    List.range 1 20
        |> List.map (\n -> n * 100)


toFunction : Param Graph Function -> () -> ()
toFunction param =
    case param.function of
        Slow ->
            \_ -> ignore <| fibSlow param.size

        Fast ->
            \_ -> ignore <| fibFast param.size


fibSlow : Int -> Int
fibSlow =
    let
        go : Dict Int Int -> Int -> ( Int, Dict Int Int )
        go acc n =
            if n < 2 then
                ( 1, acc )

            else
                case Dict.get n acc of
                    Just r ->
                        ( r, acc )

                    Nothing ->
                        let
                            ( f1, acc1 ) =
                                go acc (n - 1)

                            ( f2, acc2 ) =
                                go acc1 (n - 2)
                        in
                        ( f1 + f2, Dict.insert n (f1 + f2) acc2 )
    in
    go Dict.empty
        >> Tuple.first


fibFast : Int -> Int
fibFast n =
    let
        go : Int -> Int -> Int -> Int
        go c a b =
            if c <= 0 then
                a

            else
                go (c - 1) (a + b) a
    in
    go n 1 1


ignore : a -> ()
ignore _ =
    ()


timeout : Maybe Float
timeout =
    Just 20


type Graph
    = Simple


type Function
    = Fast
    | Slow
