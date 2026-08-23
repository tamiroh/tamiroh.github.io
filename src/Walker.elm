module Walker exposing (Look, Walker, generator, step, view)

import Geometry exposing (Position)
import Millis exposing (Millis)
import Random
import Screen exposing (Screen)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Torus exposing (Torus)
import Transform



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
        , speed : Float
        }


generator : Screen -> Random.Generator Walker
generator screen =
    Random.map2
        (\walked spd -> Walker { walked = walked, pace = spd, speed = spd })
        (Random.float 0 screen.width)
        speedGenerator


speedGenerator : Random.Generator Float
speedGenerator =
    Random.float slowest fastest


slowest : Float
slowest =
    18


fastest : Float
fastest =
    36


step : Millis -> Torus -> Float -> Maybe Position -> Walker -> Walker
step delta torus level pointer (Walker walker) =
    let
        current =
            pace walker.speed torus level walker.walked pointer
    in
    Walker
        { walked = walker.walked + current * delta / 1000
        , pace = current
        , speed = walker.speed
        }


view : Look -> Torus -> Float -> Walker -> List (Svg msg)
view look torus level (Walker walker) =
    let
        here =
            strolled torus walker.walked

        hurry =
            max 0 (walker.pace / walker.speed - 1)

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
        (Torus.copiesX width torus here)



-- PACE


pace : Float -> Torus -> Float -> Float -> Maybe Position -> Float
pace speed torus level walked pointer =
    case pointer of
        Nothing ->
            speed

        Just ( px, py ) ->
            let
                aside =
                    Torus.deltaX torus (strolled torus walked) px

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


strolled : Torus -> Float -> Float
strolled torus walked =
    Torus.wrapX torus (negate walked)


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
            (String.join " "
                [ Transform.translate ( pose.x, pose.ground )
                , Transform.rotate pose.tilt
                , Transform.scale scale
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
        , SvgAttr.transform (Transform.rotateAbout swing ( hx, hy ))
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
