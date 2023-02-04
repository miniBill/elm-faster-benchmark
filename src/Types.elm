module Types exposing (Function(..), GraphName, Index, Param, ToBackend(..), ToFrontend(..), functionToString)

import Benchmark.Parametric exposing (Stats)


type ToFrontend
    = TFParams (List Param)
    | TFResult Param (Result String Stats)


type ToBackend
    = TBParams
    | TBParam Param


type alias Index =
    Int


type alias Param =
    { graphName : GraphName
    , function : Function
    , size : Int
    }


type alias GraphName =
    String


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
