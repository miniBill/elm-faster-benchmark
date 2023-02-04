module Ui.Theme exposing (button, padding, spacing)

import Element exposing (Attribute, Element)
import Element.Border as Border
import Element.Input as Input


rythm : number
rythm =
    10


spacing : Attribute msg
spacing =
    Element.spacing rythm


padding : Attribute msg
padding =
    Element.padding rythm


button :
    List (Attribute msg)
    ->
        { label : Element msg
        , onPress : Maybe msg
        }
    -> Element msg
button attrs =
    Input.button (Border.width 1 :: Border.rounded rythm :: padding :: attrs)
