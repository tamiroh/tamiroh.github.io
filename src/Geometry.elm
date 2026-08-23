module Geometry exposing (Position, Rect, Screen, Vector, wrap, wrapDelta)

-- GEOMETRY


type alias Position =
    ( Float, Float )


type alias Vector =
    ( Float, Float )


type alias Screen =
    { width : Float
    , height : Float
    }


type alias Rect =
    { left : Float
    , top : Float
    , right : Float
    , bottom : Float
    }


wrap : Float -> Float -> Float
wrap span value =
    if span <= 0 then
        value

    else
        value - span * toFloat (floor (value / span))


wrapDelta : Float -> Float -> Float
wrapDelta span value =
    if value > span / 2 then
        value - span

    else if value < negate (span / 2) then
        value + span

    else
        value
