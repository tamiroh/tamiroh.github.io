module Walker exposing (Pose, height, view, width)

import Geometry exposing (Position)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr



-- WALKER


type alias Pose =
    { x : Float
    , ground : Float
    , swing : Float
    , tilt : Float
    , standing : Bool
    }


scale : Float
scale =
    0.07


height : Float
height =
    650 * scale


width : Float
width =
    400 * scale


view : String -> String -> Float -> Pose -> Svg msg
view ink paper stroke pose =
    Svg.g
        [ SvgAttr.transform
            (String.concat
                [ "translate("
                , num pose.x
                , ","
                , num pose.ground
                , ") rotate("
                , num pose.tilt
                , ") scale("
                , num scale
                , ")"
                ]
            )
        , SvgAttr.fill "none"
        , SvgAttr.stroke ink
        , SvgAttr.strokeWidth (num (stroke / scale))
        , SvgAttr.strokeLinecap "round"
        , SvgAttr.strokeLinejoin "round"
        ]
        (Svg.path [ SvgAttr.d body, SvgAttr.fill paper ] []
            :: eye -130
            :: eye -25
            :: legs pose
        )


legs : Pose -> List (Svg msg)
legs pose =
    if pose.standing then
        [ Svg.path [ SvgAttr.d leftStand ] []
        , Svg.path [ SvgAttr.d rightStand ] []
        ]

    else
        [ limb leftHip pose.swing leftLimb
        , limb rightHip (negate pose.swing) rightLimb
        ]


eye : Float -> Svg msg
eye offset =
    Svg.line
        [ SvgAttr.x1 (num offset)
        , SvgAttr.y1 "-462"
        , SvgAttr.x2 (num offset)
        , SvgAttr.y2 "-378"
        ]
        []


limb : Position -> Float -> String -> Svg msg
limb ( hx, hy ) swing shape =
    Svg.path
        [ SvgAttr.d shape
        , SvgAttr.transform
            (String.concat [ "rotate(", num swing, " ", num hx, " ", num hy, ")" ])
        ]
        []


num : Float -> String
num =
    String.fromFloat



-- SHAPE


body : String
body =
    String.join " "
        [ "M -200 -170"
        , "L -200 -420"
        , "C -200 -565, -105 -650, 0 -650"
        , "C 105 -650, 200 -565, 200 -420"
        , "L 200 -170"
        , "C 200 -155, 190 -150, 170 -150"
        , "L -170 -150"
        , "C -190 -150, -200 -155, -200 -170"
        , "Z"
        ]


leftHip : Position
leftHip =
    ( -90, -150 )


rightHip : Position
rightHip =
    ( 105, -150 )


leftLimb : String
leftLimb =
    "M -90 -150 L -132 -6 L -180 -20"


rightLimb : String
rightLimb =
    "M 105 -150 L 147 -6 L 99 8"


leftStand : String
leftStand =
    "M -90 -150 L -90 0 L -140 0"


rightStand : String
rightStand =
    "M 105 -150 L 105 0 L 55 0"
