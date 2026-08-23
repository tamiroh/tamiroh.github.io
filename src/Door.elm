module Door exposing (Look, view)

import Geometry exposing (Position)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr



-- DOOR


type alias Look =
    { ink : String
    , paper : String
    }


view : Look -> Float -> Position -> List (Svg msg)
view look size ( x, y ) =
    let
        midX =
            x + size / 2

        apex =
            y + size * doorTop

        bottom =
            y + size * doorBottom

        shoulder =
            apex + size * doorWidth / 2

        left =
            midX - size * doorWidth / 2

        right =
            midX + size * doorWidth / 2
    in
    [ Svg.path
        [ SvgAttr.d
            (String.join " "
                [ "M"
                , num left
                , num bottom
                , "L"
                , num left
                , num shoulder
                , "Q"
                , num left
                , num apex
                , num midX
                , num apex
                , "Q"
                , num right
                , num apex
                , num right
                , num shoulder
                , "L"
                , num right
                , num bottom
                , "Z"
                ]
            )
        , SvgAttr.fill look.paper
        ]
        []
    , Svg.circle
        [ SvgAttr.cx (num (right - size * knobInset))
        , SvgAttr.cy (num (y + size * (doorTop + doorBottom) / 2))
        , SvgAttr.r (num (size * knobRadius))
        , SvgAttr.fill look.ink
        ]
        []
    ]



-- SHAPE


doorTop : Float
doorTop =
    0.2


doorBottom : Float
doorBottom =
    0.8


doorWidth : Float
doorWidth =
    0.5


knobInset : Float
knobInset =
    0.1


knobRadius : Float
knobRadius =
    0.05


num : Float -> String
num =
    String.fromFloat
