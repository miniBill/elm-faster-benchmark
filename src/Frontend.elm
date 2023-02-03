port module Frontend exposing (Flags, Model, Msg, main)

import Browser
import Codec exposing (Value)
import Codecs
import Element exposing (Element, column, text)
import Theme
import Types exposing (ToBackend(..), ToFrontend(..))


type alias Flags =
    { workersCount : Int
    }


type alias Model =
    { workersCount : Int
    , params : Maybe (List Value)
    }


type Msg
    = FromBackend ToFrontend
    | Stop
    | Nop


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = \model -> Element.layout [] <| view model
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model : Model
        model =
            { workersCount = flags.workersCount
            , params = Nothing
            }
    in
    ( model, sendToBackend 0 TBParams )


view : Model -> Element Msg
view model =
    column [ Theme.padding, Theme.spacing ]
        [ text <| "Concurrency: " ++ String.fromInt model.workersCount
        , case model.params of
            Nothing ->
                text "Loading param list..."

            Just lst ->
                text <| "Will run with " ++ String.fromInt (List.length lst) ++ " params"
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FromBackend (TFParams params) ->
            ( { model | params = Just params }, Cmd.none )

        Stop ->
            ( model
            , List.range 0 (model.workersCount - 1)
                |> List.map terminate
                |> Cmd.batch
            )

        Nop ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveFromBackend



-- PORTS --


port terminate : Int -> Cmd msg


port fromBackend : (Value -> msg) -> Sub msg


port toBackend : { index : Int, value : Value } -> Cmd msg


sendToBackend : Int -> ToBackend -> Cmd msg
sendToBackend index tb =
    toBackend
        { index = index
        , value = Codec.encodeToValue Codecs.toBackendCodec tb
        }


receiveFromBackend : Sub Msg
receiveFromBackend =
    fromBackend
        (\value ->
            case Codec.decodeValue Codecs.toFrontendCodec value of
                Ok decoded ->
                    FromBackend decoded

                Err _ ->
                    Nop
        )
