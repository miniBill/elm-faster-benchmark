module Codecs exposing (toBackendCodec, toFrontendCodec)

import Codec exposing (Codec)
import Types exposing (ToBackend(..), ToFrontend(..))


toFrontendCodec : Codec ToFrontend
toFrontendCodec =
    Codec.custom
        (\params value ->
            case value of
                TFParams i ->
                    params i
        )
        |> Codec.variant1 "TFParams" TFParams (Codec.list Codec.value)
        |> Codec.buildCustom


toBackendCodec : Codec ToBackend
toBackendCodec =
    Codec.custom
        (\params nop value ->
            case value of
                TBParams ->
                    params

                TBNop ->
                    nop
        )
        |> Codec.variant0 "TBParams" TBParams
        |> Codec.variant0 "TBNop" TBNop
        |> Codec.buildCustom
