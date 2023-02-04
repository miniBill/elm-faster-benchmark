module Frontend.OkLch exposing (Float3, oklchToSRGB)


oklchToSRGB : Float3 -> Float3
oklchToSRGB =
    oklchToOklab >> oklabToLinear >> triple linearToSRGB


type alias Float3 =
    ( Float, Float, Float )


triple : (a -> b) -> ( a, a, a ) -> ( b, b, b )
triple f ( r, g, b ) =
    ( f r, f g, f b )


linearToSRGB : Float -> Float
linearToSRGB l =
    -- Higher precision constant from https://entropymine.com/imageworsener/srgbformula/
    if l <= 0.00313066844250063 then
        l * 12.92

    else
        1.055 * l ^ (1 / 2.4) - 0.055


oklabToLinear : Float3 -> Float3
oklabToLinear ( l, a, b ) =
    let
        l_ : Float
        l_ =
            l + 0.3963377774 * a + 0.2158037573 * b

        m_ : Float
        m_ =
            l - 0.1055613458 * a - 0.0638541728 * b

        s_ : Float
        s_ =
            l - 0.0894841775 * a - 1.291485548 * b

        lOut : Float
        lOut =
            l_ * l_ * l_

        m : Float
        m =
            m_ * m_ * m_

        s : Float
        s =
            s_ * s_ * s_
    in
    ( 4.0767416621 * lOut - 3.3077115913 * m + 0.2309699292 * s
    , -1.2684380046 * lOut + 2.6097574011 * m - 0.3413193965 * s
    , -0.0041960863 * lOut - 0.7034186147 * m + 1.707614701 * s
    )


oklchToOklab : Float3 -> Float3
oklchToOklab ( l, c, h ) =
    let
        a : Float
        a =
            c * cos (degrees h)

        b : Float
        b =
            c * sin (degrees h)
    in
    ( l, a, b )
