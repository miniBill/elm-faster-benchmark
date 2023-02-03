port module Backend exposing (Flags, Model, Msg, main)

import Codec exposing (Value)
import Codecs
import Types exposing (ToBackend(..), ToFrontend(..))


type alias Flags =
    {}


type alias Model =
    {}


type Msg
    = FromFrontend ToBackend
    | Nop


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FromFrontend TBParams ->
            ( model, sendToFrontend <| TFParams [ Codec.encodeToValue Codec.int 0 ] )

        Nop ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveFromFrontend



-- PORTS --


port fromFrontend : (Value -> msg) -> Sub msg


port toFrontend : Value -> Cmd msg


sendToFrontend : ToFrontend -> Cmd msg
sendToFrontend tf =
    toFrontend (Codec.encodeToValue Codecs.toFrontendCodec tf)


receiveFromFrontend : Sub Msg
receiveFromFrontend =
    fromFrontend
        (\value ->
            case Codec.decodeValue Codecs.toBackendCodec value of
                Ok decoded ->
                    FromFrontend decoded

                Err _ ->
                    Nop
        )
