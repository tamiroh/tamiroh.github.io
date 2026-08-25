module Piece exposing (view)

import Geometry exposing (Position)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr


view : Float -> Position -> String -> String -> Bool -> Svg msg
view radius ( x, y ) ink paper dark =
    Svg.circle
        [ SvgAttr.cx (String.fromFloat x)
        , SvgAttr.cy (String.fromFloat y)
        , SvgAttr.r (String.fromFloat radius)
        , SvgAttr.fill
            (if dark then
                ink

             else
                paper
            )
        , SvgAttr.stroke ink
        ]
        []
