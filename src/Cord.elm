module Cord exposing (Look, Pull, step, view)

import Cursor
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Millis exposing (Millis)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr


type alias Look =
    { ink : String
    , paper : String
    , stroke : Float
    }


type alias Pull =
    { elapsed : Millis }


view : Look -> msg -> Maybe Pull -> Html msg
view look pulled pull =
    Html.div
        [ Attr.style "position" "fixed"
        , Attr.style "top" "0"
        , Attr.style "right" (String.fromFloat inset ++ "px")
        , Attr.style "cursor" (Cursor.css 1 look Cursor.Clickable)
        , Attr.style "user-select" "none"
        , Html.Events.onClick pulled
        ]
        [ hanging look (pullOffset pull) ]


hanging : Look -> Float -> Svg msg
hanging look dy =
    Svg.svg
        [ SvgAttr.width (String.fromFloat width)
        , SvgAttr.height (String.fromFloat (length + gripHeight + look.stroke))
        , SvgAttr.style "overflow: visible"
        ]
        [ Svg.line
            [ SvgAttr.x1 (String.fromFloat (width / 2))
            , SvgAttr.y1 "0"
            , SvgAttr.x2 (String.fromFloat (width / 2))
            , SvgAttr.y2 (String.fromFloat (length + dy))
            , SvgAttr.stroke look.ink
            , SvgAttr.strokeWidth (String.fromFloat look.stroke)
            ]
            []
        , Svg.rect
            [ SvgAttr.x (String.fromFloat ((width - gripWidth) / 2))
            , SvgAttr.y (String.fromFloat (length + dy))
            , SvgAttr.width (String.fromFloat gripWidth)
            , SvgAttr.height (String.fromFloat gripHeight)
            , SvgAttr.rx (String.fromFloat (gripWidth / 2))
            , SvgAttr.fill look.paper
            , SvgAttr.stroke look.ink
            , SvgAttr.strokeWidth (String.fromFloat look.stroke)
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


pullAmplitude : Float
pullAmplitude =
    20


pullDuration : Millis
pullDuration =
    600


step : Millis -> Maybe Pull -> Maybe Pull
step delta pull =
    case pull of
        Nothing ->
            Nothing

        Just current ->
            let
                elapsed =
                    current.elapsed + delta
            in
            if elapsed > pullDuration then
                Nothing

            else
                Just { current | elapsed = elapsed }


pullCycles : Float
pullCycles =
    1.25


pullDamping : Float
pullDamping =
    4


pullOffset : Maybe Pull -> Float
pullOffset pull =
    case pull of
        Nothing ->
            0

        Just { elapsed } ->
            let
                phase =
                    elapsed / pullDuration
            in
            pullAmplitude
                * sin (pullCycles * 2 * pi * phase)
                * e
                ^ negate (pullDamping * phase)
