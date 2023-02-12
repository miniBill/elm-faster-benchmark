module ToBenchmark exposing (Function, Graph, config)

import Array exposing (Array)
import Codec exposing (Codec)
import FastBenchmark.Types exposing (Config, Param)


config : Config Graph Function
config =
    { graphTitle = graphTitle
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


type Graph
    = Simple


graphs : List Graph
graphs =
    [ Simple ]


graphTitle : Graph -> String
graphTitle graph =
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


type Function
    = Fast
    | Slow


functions : List Function
functions =
    [ Slow
    , Fast
    ]


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
        go : Array Int -> Int -> ( Int, Array Int )
        go acc n =
            case Array.get n acc of
                Just r ->
                    ( r, acc )

                Nothing ->
                    let
                        ( f1, acc1 ) =
                            go acc (n - 1)

                        ( f2, acc2 ) =
                            go acc1 (n - 2)
                    in
                    ( f1 + f2, Array.push (f1 + f2) acc2 )
    in
    go (Array.fromList [ 1, 1 ])
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
