module Bird exposing (Look, view)

import Geometry exposing (Position)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr



-- BIRD


type alias Look =
    { ink : String
    , paper : String
    , stroke : Float
    }


type alias Pose =
    { x : Float
    , y : Float
    , heading : Float
    }


width : Float
width =
    14


view : Look -> Pose -> Svg msg
view look pose =
    let
        spot : Position -> String
        spot ( x, y ) =
            num x ++ "," ++ num y

        outline : String
        outline =
            String.join " " (List.map spot shape)
    in
    Svg.polygon
        [ SvgAttr.points outline
        , SvgAttr.fill look.paper
        , SvgAttr.stroke look.ink
        , SvgAttr.strokeWidth (String.fromFloat (look.stroke / width))
        , SvgAttr.strokeLinejoin "round"
        , SvgAttr.transform
            (String.concat
                [ "translate("
                , num pose.x
                , ","
                , num pose.y
                , ") rotate("
                , num (pose.heading + 90)
                , ") scale("
                , num width
                , ")"
                ]
            )
        ]
        []


num : Float -> String
num =
    String.fromFloat



-- SHAPE


shape : List Position
shape =
    [ ( -0.03, -0.422 )
    , ( 0.004, -0.409 )
    , ( 0.065, -0.409 )
    , ( 0.083, -0.378 )
    , ( 0.152, -0.335 )
    , ( 0.187, -0.27 )
    , ( 0.209, -0.17 )
    , ( 0.322, -0.126 )
    , ( 0.391, -0.074 )
    , ( 0.478, 0.03 )
    , ( 0.5, 0.091 )
    , ( 0.5, 0.183 )
    , ( 0.491, 0.187 )
    , ( 0.17, 0.187 )
    , ( 0.157, 0.209 )
    , ( 0.143, 0.287 )
    , ( 0.113, 0.348 )
    , ( 0.048, 0.413 )
    , ( 0.03, 0.409 )
    , ( 0.009, 0.422 )
    , ( -0.096, 0.304 )
    , ( -0.143, 0.174 )
    , ( -0.3, 0.157 )
    , ( -0.5, 0.157 )
    , ( -0.465, 0.052 )
    , ( -0.4, -0.074 )
    , ( -0.265, -0.178 )
    , ( -0.226, -0.196 )
    , ( -0.161, -0.2 )
    , ( -0.1, -0.322 )
    , ( -0.03, -0.391 )
    , ( -0.03, -0.422 )
    ]
