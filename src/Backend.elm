port module Backend exposing (main)

import Backend.App
import Codec exposing (Value)
import ToBenchmark exposing (Function, Graph)


main : Backend.App.Program Graph Function
main =
    Backend.App.app ToBenchmark.config ports


ports : Backend.App.Ports msg
ports =
    { fromFrontend = fromFrontend
    , toFrontend = toFrontend
    }


port fromFrontend : (Value -> msg) -> Sub msg


port toFrontend : Value -> Cmd msg
