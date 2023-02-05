module FastBenchmark.Frontend.Theme exposing (borderRounded, borderWidth, button, padding, spacing)

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


borderRounded : Attribute msg
borderRounded =
    Border.rounded rythm


borderWidth : Attribute msg
borderWidth =
    Border.width 1


button :
    List (Attribute msg)
    ->
        { label : Element msg
        , onPress : Maybe msg
        }
    -> Element msg
button attrs =
    Input.button (borderRounded :: borderWidth :: padding :: attrs)
