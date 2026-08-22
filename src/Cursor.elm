module Cursor exposing (css)

-- CURSOR


radius : Float
radius =
    11


css : String -> String -> Float -> Bool -> String
css ink paper stroke active =
    let
        edge =
            radius + stroke

        skin =
            if active then
                ink

            else
                paper

        face =
            if active then
                paper

            else
                ink
    in
    String.concat
        [ "url(\"data:image/svg+xml,"
        , "%3Csvg xmlns='http://www.w3.org/2000/svg' width='"
        , num (edge * 2)
        , "' height='"
        , num (edge * 2)
        , "'%3E%3Cg stroke='"
        , webColour ink
        , "' stroke-width='"
        , num stroke
        , "' stroke-linecap='round'%3E%3Ccircle cx='"
        , num edge
        , "' cy='"
        , num edge
        , "' r='"
        , num radius
        , "' fill='"
        , webColour skin
        , "'/%3E"
        , wink face (edge - radius * 0.35) (edge - radius * 0.28)
        , wink face (edge + radius * 0.35) (edge - radius * 0.28)
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


wink : String -> Float -> Float -> String
wink colour x y =
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
