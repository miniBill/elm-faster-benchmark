port module Frontend exposing (main)

import Codec exposing (Value)
import Frontend.App
import ToBenchmark exposing (Function, Graph)


main : Frontend.App.Program Graph Function
main =
    Frontend.App.app ToBenchmark.config ports


ports : Frontend.App.Ports msg
ports =
    { terminateAll = terminateAll
    , fromBackend = fromBackend
    , toBackend = toBackend
    }


port terminateAll : {} -> Cmd msg


port fromBackend : ({ index : Int, data : Value } -> msg) -> Sub msg


port toBackend : { index : Int, data : Value } -> Cmd msg
