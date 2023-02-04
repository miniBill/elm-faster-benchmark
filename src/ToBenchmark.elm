module ToBenchmark exposing (Function, Graph, config)

import Codec exposing (Codec)
import Common.Types exposing (Config, Param)
import Dict exposing (Dict)
import DictDotDot as DDD
import FastIntersect
import Random


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
    = Library
    | Alternative


functionToString : Function -> String
functionToString function =
    case function of
        Library ->
            "elm/core"

        Alternative ->
            "mine"


functionCodec : Codec Function
functionCodec =
    Codec.custom
        (\flibrary falt value ->
            case value of
                Library ->
                    flibrary

                Alternative ->
                    falt
        )
        |> Codec.variant0 "Library" Library
        |> Codec.variant0 "Alternative" Alternative
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
    [ Library
    , Alternative
    ]


sizes : List Int
sizes =
    List.range 1 14
        |> List.map (\i -> 100 * 3 ^ i // 2 ^ i)


type alias Both k v =
    { core : Dict k v
    , dotdot : DDD.Dict k v
    }


{-| `generate n` generates a list of n numbers between 0 and 2n
-}
generate : Int -> Both Int Int
generate size =
    let
        generator : Random.Generator (Both Int Int)
        generator =
            Random.int 0 (2 * size)
                |> Random.map (\t -> ( t, t ))
                |> Random.list size
                |> Random.map (\lst -> { core = Dict.fromList lst, dotdot = DDD.fromList lst })
    in
    Random.step generator (Random.initialSeed size)
        |> Tuple.first


toFunction : Param Graph Function -> (() -> ())
toFunction { function, size } =
    case function of
        Library ->
            ignore << library size

        Alternative ->
            ignore << alternative size


library : Int -> () -> Dict Int Int
library size =
    let
        left : Dict Int Int
        left =
            (generate size).core

        right : Dict Int Int
        right =
            (generate (size + 1)).core
    in
    \_ -> Dict.intersect left right


alternative : Int -> () -> DDD.Dict Int Int
alternative size =
    let
        left : DDD.Dict Int Int
        left =
            (generate size).dotdot

        right : DDD.Dict Int Int
        right =
            (generate (size + 1)).dotdot
    in
    \_ -> FastIntersect.intersect left right


ignore : a -> ()
ignore _ =
    ()
