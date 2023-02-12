module FastBenchmark.Types exposing
    ( Stats, Param
    , ToFrontend(..), ToBackend(..), Index
    )

{-|


# Types

@docs Config, Stats, Param


# Messages

@docs ToFrontend, ToBackend, Index

-}


{-| Statistics un run times.
-}
type alias Stats =
    { firstQuartile : Float
    , median : Float
    , thirdQuartile : Float
    , max : Float
    , min : Float
    , outliers : List Float
    }


{-| Message that gets sent to the frontend.
-}
type ToFrontend graph function
    = TFParams { timeout : Maybe Float, params : List (Param graph function) }
    | TFResult (Param graph function) (Result String Stats)


{-| Message that gets sent to the backend.
-}
type ToBackend graph function
    = TBParams
    | TBRun (Param graph function)


{-| Index of the Web Worker.
-}
type alias Index =
    Int


{-| Parameters needed to measure the timing.
-}
type alias Param graph function =
    { graph : graph
    , function : function
    , size : Int
    }
