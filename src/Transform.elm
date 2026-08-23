module Transform exposing (rotate, rotateAbout, scale, translate)

import Geometry exposing (Position)


translate : Position -> String
translate ( x, y ) =
    "translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")"


rotate : Float -> String
rotate degrees =
    "rotate(" ++ String.fromFloat degrees ++ ")"


rotateAbout : Float -> Position -> String
rotateAbout degrees ( x, y ) =
    "rotate(" ++ String.fromFloat degrees ++ " " ++ String.fromFloat x ++ " " ++ String.fromFloat y ++ ")"


scale : Float -> String
scale factor =
    "scale(" ++ String.fromFloat factor ++ ")"
