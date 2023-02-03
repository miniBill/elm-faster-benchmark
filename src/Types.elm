module Types exposing (ToBackend(..), ToFrontend(..))

import Codec exposing (Value)


type ToFrontend
    = TFParams (List Value)


type ToBackend
    = TBParams
    | TBNop
