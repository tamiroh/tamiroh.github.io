module Ground exposing (level, view)

import Html exposing (Html)
import Html.Attributes as Attr
import Millis exposing (Millis)
import Screen exposing (Screen)
import Svg
import Svg.Attributes as SvgAttr
import Torus
import Walker exposing (Walker)


type alias Look =
    { ink : String
    , paper : String
    , stroke : Float
    }


depth : Float
depth =
    52


level : Screen -> Float
level screen =
    screen.height - depth


view : Look -> Millis -> Screen -> List Walker -> Html msg
view look now screen walkers =
    Svg.svg
        [ SvgAttr.width (String.fromFloat screen.width)
        , SvgAttr.height (String.fromFloat screen.height)
        , Attr.style "position" "fixed"
        , Attr.style "inset" "0"
        , Attr.style "pointer-events" "none"
        ]
        (Svg.rect
            [ SvgAttr.x "0"
            , SvgAttr.y (String.fromFloat (level screen))
            , SvgAttr.width (String.fromFloat screen.width)
            , SvgAttr.height (String.fromFloat depth)
            , SvgAttr.fill look.paper
            ]
            []
            :: Svg.line
                [ SvgAttr.x1 "0"
                , SvgAttr.y1 (String.fromFloat (level screen))
                , SvgAttr.x2 (String.fromFloat screen.width)
                , SvgAttr.y2 (String.fromFloat (level screen))
                , SvgAttr.stroke look.ink
                , SvgAttr.strokeWidth (String.fromFloat look.stroke)
                ]
                []
            :: List.concatMap (Walker.view look now (Torus.around screen) (level screen)) walkers
        )
