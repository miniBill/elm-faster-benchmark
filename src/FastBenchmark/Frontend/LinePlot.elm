module FastBenchmark.Frontend.LinePlot exposing (Data, Datum, view)

import Axis
import Color exposing (Color)
import Dict exposing (Dict)
import FastBenchmark.Types exposing (Stats)
import Html.Attributes
import Path exposing (Path)
import Scale exposing (ContinuousScale)
import Shape
import TypedSvg exposing (circle, g, line, svg, text_)
import TypedSvg.Attributes exposing (class, fill, opacity, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, fontSize, r, strokeWidth, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Opacity(..), Paint(..), Transform(..))


type alias Data =
    List Datum


type alias Datum =
    ( Color, Dict Int Stats )


w : Float
w =
    700


h : Float
h =
    w / 2


padding : Float
padding =
    30


xScale : Data -> ContinuousScale Float
xScale model =
    let
        allPoints : List Int
        allPoints =
            model
                |> List.concatMap (\( _, points ) -> Dict.keys points)

        min : Int
        min =
            allPoints
                |> List.minimum
                |> Maybe.withDefault 200

        max : Int
        max =
            allPoints
                |> List.maximum
                |> Maybe.withDefault 200
    in
    Scale.linear ( 0, w - 2 * padding ) ( toFloat min, toFloat max )


yScale : Float -> ContinuousScale Float
yScale max =
    Scale.linear ( h - 2 * padding, 0 ) ( 0, max )


xAxis : Data -> Svg msg
xAxis model =
    Axis.bottom [] (xScale model)


yAxis : Float -> Svg msg
yAxis max =
    g []
        [ Axis.left [ Axis.tickCount 9 ] (yScale max)
        , text_
            [ fontSize 14
            , x -padding
            , y <| -padding / 2
            ]
            [ TypedSvg.Core.text "Duration (ms)" ]
        ]


column : Float -> ContinuousScale Float -> Color -> Dict Int Stats -> List (Svg msg)
column max scale color statsDict =
    let
        statsList : List ( Int, Stats )
        statsList =
            Dict.toList statsDict

        transformToLineData : (Stats -> Float) -> ( Int, Stats ) -> Maybe ( Float, Float )
        transformToLineData selector ( x, y ) =
            Just ( Scale.convert scale (toFloat x), Scale.convert (yScale max) (selector y) )

        tranfromToAreaData : ( Int, Stats ) -> Maybe ( ( Float, Float ), ( Float, Float ) )
        tranfromToAreaData ( x, y ) =
            Just
                ( ( Scale.convert scale (toFloat x), Scale.convert (yScale max) y.min )
                , ( Scale.convert scale (toFloat x), Scale.convert (yScale max) y.max )
                )

        line : (Stats -> Float) -> List ( Int, Stats ) -> Path
        line selector model =
            List.map (transformToLineData selector) model
                |> Shape.line Shape.monotoneInXCurve

        area : List ( Int, Stats ) -> Path
        area model =
            List.map tranfromToAreaData model
                |> Shape.area Shape.monotoneInXCurve

        qtColor : Color
        qtColor =
            color
                |> Color.toRgba
                |> (\rgba -> { rgba | alpha = 0.5 })
                |> Color.fromRgba

        bgColor : Color
        bgColor =
            color
                |> Color.toRgba
                |> (\rgba -> { rgba | alpha = 0.25 })
                |> Color.fromRgba

        outlierCircle : Float -> Float -> Svg msg
        outlierCircle x y =
            circle
                [ cx x
                , cy y
                , r 2
                , fill <| Paint color
                , stroke <| PaintNone
                , opacity <| Opacity 0.5
                ]
                []

        outliers : List (Svg msg)
        outliers =
            statsList
                |> List.concatMap (\( size, ss ) -> List.map (Scale.convert (yScale max) >> outlierCircle (Scale.convert scale (toFloat size))) ss.outliers)
    in
    [ Path.element (area statsList)
        [ stroke <| Paint color
        , strokeWidth 1
        , fill <| Paint bgColor
        ]
    , Path.element (line .median statsList)
        [ stroke <| Paint color
        , strokeWidth 1
        , fill PaintNone
        ]
    , Path.element (line .firstQuartile statsList)
        [ stroke <| Paint qtColor
        , strokeWidth 0.5
        , fill PaintNone
        ]
    , Path.element (line .thirdQuartile statsList)
        [ stroke <| Paint qtColor
        , strokeWidth 0.5
        , fill PaintNone
        ]
    ]
        ++ outliers


yGridLine : Float -> Int -> Float -> Svg msg
yGridLine max index tick =
    line
        [ x1 0
        , x2 (w - 2 * padding)
        , y1 (Scale.convert (yScale max) tick)
        , y2 (Scale.convert (yScale max) tick)
        , stroke <| Paint Color.black
        , strokeWidth (toFloat (modBy 2 index) * 0.25 + 0.25)
        , opacity <| Opacity 0.3
        ]
        []


view : Data -> Svg msg
view model =
    let
        max : Float
        max =
            model
                |> List.concatMap (\( _, times ) -> Dict.values times)
                |> List.map .max
                |> List.maximum
                |> Maybe.withDefault 0
    in
    svg [ viewBox 0 0 w h, Html.Attributes.style "width" <| String.fromFloat w ++ "px" ]
        [ g [ transform [ Translate padding (padding + 0.5) ] ] <| List.indexedMap (yGridLine max) <| Scale.ticks (yScale max) 9
        , g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis model ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis max ]
        , model
            |> List.concatMap (\( color, datum ) -> column max (xScale model) color datum)
            |> g [ transform [ Translate padding padding ], class [ "series" ] ]
        ]
