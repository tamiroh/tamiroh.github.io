module Cord exposing (view)

import Cursor
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr



-- CORD


view : String -> String -> Float -> msg -> Float -> Html msg
view ink paper stroke pulled dy =
    Html.div
        [ Attr.style "position" "fixed"
        , Attr.style "top" "0"
        , Attr.style "right" (String.fromFloat inset ++ "px")
        , Attr.style "cursor" (Cursor.css ink paper stroke True)
        , Attr.style "user-select" "none"
        , Html.Events.onClick pulled
        ]
        [ hanging ink paper stroke dy ]


hanging : String -> String -> Float -> Float -> Svg msg
hanging ink paper stroke dy =
    Svg.svg
        [ SvgAttr.width (String.fromFloat width)
        , SvgAttr.height (String.fromFloat (length + gripHeight + stroke))
        , SvgAttr.style "overflow: visible"
        ]
        [ Svg.line
            [ SvgAttr.x1 (String.fromFloat (width / 2))
            , SvgAttr.y1 "0"
            , SvgAttr.x2 (String.fromFloat (width / 2))
            , SvgAttr.y2 (String.fromFloat (length + dy))
            , SvgAttr.stroke ink
            , SvgAttr.strokeWidth (String.fromFloat stroke)
            ]
            []
        , Svg.rect
            [ SvgAttr.x (String.fromFloat ((width - gripWidth) / 2))
            , SvgAttr.y (String.fromFloat (length + dy))
            , SvgAttr.width (String.fromFloat gripWidth)
            , SvgAttr.height (String.fromFloat gripHeight)
            , SvgAttr.rx (String.fromFloat (gripWidth / 2))
            , SvgAttr.fill paper
            , SvgAttr.stroke ink
            , SvgAttr.strokeWidth (String.fromFloat stroke)
            ]
            []
        ]


inset : Float
inset =
    64


length : Float
length =
    96


width : Float
width =
    16


gripWidth : Float
gripWidth =
    9


gripHeight : Float
gripHeight =
    16
