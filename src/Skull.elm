module Skull exposing (view)

import Geometry exposing (Position)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr



-- SKULL


type alias Look =
    { ink : String
    , paper : String
    }


view : Look -> Float -> Position -> List (Svg msg)
view look size ( x, y ) =
    let
        midX =
            x + size / 2

        midY =
            y + size / 2
    in
    [ Svg.circle
        [ SvgAttr.cx (String.fromFloat midX)
        , SvgAttr.cy (String.fromFloat (midY - lift size))
        , SvgAttr.r (String.fromFloat (radius size))
        , SvgAttr.fill look.paper
        ]
        []
    , Svg.rect
        [ SvgAttr.x (String.fromFloat (midX - jawWidth size / 2))
        , SvgAttr.y (String.fromFloat (midY + jawTop size))
        , SvgAttr.width (String.fromFloat (jawWidth size))
        , SvgAttr.height (String.fromFloat (jawHeight size))
        , SvgAttr.rx (String.fromFloat (jawHeight size / 3))
        , SvgAttr.fill look.paper
        ]
        []
    , socket look size ( midX - eyeGap size, midY )
    , socket look size ( midX + eyeGap size, midY )
    , tooth look size ( midX - toothGap size, midY )
    , tooth look size ( midX + toothGap size, midY )
    ]


socket : Look -> Float -> Position -> Svg msg
socket look size ( midX, midY ) =
    Svg.circle
        [ SvgAttr.cx (String.fromFloat midX)
        , SvgAttr.cy (String.fromFloat (midY - eyeLift size))
        , SvgAttr.r (String.fromFloat (eyeRadius size))
        , SvgAttr.fill look.ink
        ]
        []


tooth : Look -> Float -> Position -> Svg msg
tooth look size ( midX, midY ) =
    Svg.rect
        [ SvgAttr.x (String.fromFloat (midX - toothWidth size / 2))
        , SvgAttr.y (String.fromFloat (midY + toothTop size))
        , SvgAttr.width (String.fromFloat (toothWidth size))
        , SvgAttr.height (String.fromFloat (toothHeight size))
        , SvgAttr.fill look.ink
        ]
        []



-- SHAPE


radius : Float -> Float
radius size =
    size * 0.26


lift : Float -> Float
lift size =
    size * 0.06


jawWidth : Float -> Float
jawWidth size =
    size * 0.32


jawHeight : Float -> Float
jawHeight size =
    size * 0.17


jawTop : Float -> Float
jawTop size =
    size * 0.1


eyeRadius : Float -> Float
eyeRadius size =
    size * 0.095


eyeGap : Float -> Float
eyeGap size =
    size * 0.105


eyeLift : Float -> Float
eyeLift size =
    size * 0.07


toothWidth : Float -> Float
toothWidth size =
    size * 0.044


toothHeight : Float -> Float
toothHeight size =
    size * 0.09


toothTop : Float -> Float
toothTop size =
    size * 0.13


toothGap : Float -> Float
toothGap size =
    size * 0.055
