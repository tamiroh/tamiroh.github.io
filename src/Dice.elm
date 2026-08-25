module Dice exposing (view)

import Geometry exposing (Position)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr


view : Float -> Position -> String -> Int -> List (Svg msg)
view span origin fill count =
    List.map (dot span origin fill) (cells count)


dot : Float -> Position -> String -> ( Int, Int ) -> Svg msg
dot span ( x, y ) fill ( column, row ) =
    Svg.circle
        [ SvgAttr.cx (String.fromFloat (x + offset span column))
        , SvgAttr.cy (String.fromFloat (y + offset span row))
        , SvgAttr.r (String.fromFloat (radius span))
        , SvgAttr.fill fill
        , SvgAttr.stroke "none"
        ]
        []


offset : Float -> Int -> Float
offset span index =
    span * (0.25 + 0.25 * toFloat index)


radius : Float -> Float
radius span =
    span / 14


cells : Int -> List ( Int, Int )
cells count =
    let
        corners =
            [ ( 0, 0 ), ( 2, 0 ), ( 0, 2 ), ( 2, 2 ) ]

        sides =
            [ ( 0, 1 ), ( 2, 1 ) ]

        center =
            [ ( 1, 1 ) ]
    in
    case count of
        1 ->
            center

        2 ->
            [ ( 0, 0 ), ( 2, 2 ) ]

        3 ->
            [ ( 0, 0 ), ( 1, 1 ), ( 2, 2 ) ]

        4 ->
            corners

        5 ->
            corners ++ center

        6 ->
            corners ++ sides

        7 ->
            corners ++ sides ++ center

        8 ->
            corners ++ sides ++ [ ( 1, 0 ), ( 1, 2 ) ]

        _ ->
            []
