module Cursor exposing (Look, Over(..), css)

import Geometry exposing (Position)


type alias Look =
    { ink : String
    , paper : String
    , stroke : Float
    }


radius : Float
radius =
    11


type Over
    = Empty
    | Clickable


css : Look -> Over -> String
css look over =
    let
        edge =
            radius + look.stroke

        skin =
            case over of
                Clickable ->
                    look.ink

                Empty ->
                    look.paper

        face =
            case over of
                Clickable ->
                    look.paper

                Empty ->
                    look.ink
    in
    String.concat
        [ "url(\"data:image/svg+xml,"
        , "%3Csvg xmlns='http://www.w3.org/2000/svg' width='"
        , num (edge * 2)
        , "' height='"
        , num (edge * 2)
        , "'%3E%3Cg stroke='"
        , webColour look.ink
        , "' stroke-width='"
        , num look.stroke
        , "' stroke-linecap='round'%3E%3Ccircle cx='"
        , num edge
        , "' cy='"
        , num edge
        , "' r='"
        , num radius
        , "' fill='"
        , webColour skin
        , "'/%3E"
        , wink face ( edge - radius * 0.35, edge - radius * 0.28 )
        , wink face ( edge + radius * 0.35, edge - radius * 0.28 )
        , "%3Cpath d='M "
        , num (edge - radius * 0.42)
        , " "
        , num (edge + radius * 0.12)
        , " Q "
        , num edge
        , " "
        , num (edge + radius * 0.6)
        , " "
        , num (edge + radius * 0.42)
        , " "
        , num (edge + radius * 0.12)
        , "' fill='none' stroke='"
        , webColour face
        , "'/%3E%3C/g%3E%3C/svg%3E"
        , "\") "
        , num edge
        , " "
        , num edge
        , ", auto"
        ]


wink : String -> Position -> String
wink colour ( x, y ) =
    String.concat
        [ "%3Ccircle cx='"
        , num x
        , "' cy='"
        , num y
        , "' r='"
        , num (radius * 0.13)
        , "' fill='"
        , webColour colour
        , "' stroke='none'/%3E"
        ]


num : Float -> String
num =
    String.fromFloat


webColour : String -> String
webColour =
    String.replace "#" "%23"
