module Walker exposing (Look, Walker, start, step, view)

import Geometry exposing (Position, Screen, wrap, wrapDelta)
import Millis exposing (Millis)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr



-- WALKER


type alias Look =
    { ink : String
    , paper : String
    , stroke : Float
    }


type Walker
    = Walker
        { walked : Float
        , pace : Float
        }


start : Walker
start =
    Walker { walked = 0, pace = speed }


step : Millis -> Screen -> Float -> Maybe Position -> Walker -> Walker
step delta screen level pointer (Walker walker) =
    let
        current =
            pace screen level walker.walked pointer
    in
    Walker
        { walked = walker.walked + current * delta / 1000
        , pace = current
        }


view : Look -> Screen -> Float -> Walker -> List (Svg msg)
view look screen level (Walker walker) =
    let
        here =
            strolled screen walker.walked

        hurry =
            max 0 (walker.pace / speed - 1)

        swing =
            sin (walker.walked / stride * pi) * sway * min flail (1 + hurry * 0.4)

        tilt =
            negate (min pitch (hurry * 3))
    in
    List.map
        (\x ->
            figure look
                { x = x
                , ground = level - lift
                , swing = swing
                , tilt = tilt
                , standing = walker.pace <= 0
                }
        )
        (places screen.width here)



-- PACE


pace : Screen -> Float -> Float -> Maybe Position -> Float
pace screen level walked pointer =
    case pointer of
        Nothing ->
            speed

        Just ( px, py ) ->
            let
                aside =
                    wrapDelta screen.width (strolled screen walked - px)

                above =
                    level - height / 2 - py

                span =
                    sqrt (aside * aside + above * above)

                near =
                    max 0 (1 - span / sense)

                lean =
                    negate (clamp -1 1 (aside / focus))

                dread =
                    panic * max 0 ((panicRange / max panicFloor span) ^ 2 - 1)
            in
            min (speed * limit)
                (max 0
                    (speed
                        * (1 - near * brake * max 0 (negate lean) + near * rush * max 0 lean)
                    )
                    + speed
                    * dread
                    * max 0 lean
                )


strolled : Screen -> Float -> Float
strolled screen walked =
    wrap screen.width (screen.width - walked)


places : Float -> Float -> List Float
places span here =
    if here < width then
        [ here, here + span ]

    else if here > span - width then
        [ here, here - span ]

    else
        [ here ]


speed : Float
speed =
    26


stride : Float
stride =
    26


sway : Float
sway =
    10


sense : Float
sense =
    220


rush : Float
rush =
    4


flail : Float
flail =
    2.8


pitch : Float
pitch =
    16


lift : Float
lift =
    3


panic : Float
panic =
    3


panicRange : Float
panicRange =
    60


panicFloor : Float
panicFloor =
    12


limit : Float
limit =
    30


focus : Float
focus =
    40


brake : Float
brake =
    1.8



-- FIGURE


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


figure : Look -> Pose -> Svg msg
figure look pose =
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
        , SvgAttr.stroke look.ink
        , SvgAttr.strokeWidth (num (look.stroke / scale))
        , SvgAttr.strokeLinecap "round"
        , SvgAttr.strokeLinejoin "round"
        ]
        (Svg.path [ SvgAttr.d body, SvgAttr.fill look.paper ] []
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
