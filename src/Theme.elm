module Theme exposing (padding, rythm, spacing)

import Element exposing (Attribute)


rythm : number
rythm =
    10


spacing : Attribute msg
spacing =
    Element.spacing rythm


padding : Attribute msg
padding =
    Element.padding rythm
