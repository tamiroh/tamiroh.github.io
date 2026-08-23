module Torus exposing (Torus, around, copies, copiesX, delta, deltaX, wrap, wrapX)

import Geometry exposing (Position, Vector)
import Screen exposing (Screen)



-- TORUS


type Torus
    = Torus { width : Float, height : Float }


around : Screen -> Torus
around screen =
    Torus { width = screen.width, height = screen.height }


wrap : Torus -> Position -> Position
wrap (Torus span) ( x, y ) =
    ( fold span.width x, fold span.height y )


delta : Torus -> Position -> Position -> Vector
delta (Torus span) ( ax, ay ) ( bx, by ) =
    ( nearest span.width (ax - bx), nearest span.height (ay - by) )


copies : Float -> Torus -> Position -> List Position
copies margin (Torus span) ( x, y ) =
    List.concatMap
        (\cx -> List.map (Tuple.pair cx) (beside margin span.height y))
        (beside margin span.width x)



-- ONE AXIS


wrapX : Torus -> Float -> Float
wrapX (Torus span) x =
    fold span.width x


deltaX : Torus -> Float -> Float -> Float
deltaX (Torus span) a b =
    nearest span.width (a - b)


copiesX : Float -> Torus -> Float -> List Float
copiesX margin (Torus span) x =
    beside margin span.width x



-- SPAN


fold : Float -> Float -> Float
fold span value =
    if span <= 0 then
        value

    else
        value - span * toFloat (floor (value / span))


nearest : Float -> Float -> Float
nearest span value =
    if value > span / 2 then
        value - span

    else if value < negate (span / 2) then
        value + span

    else
        value


beside : Float -> Float -> Float -> List Float
beside margin span value =
    if value < margin then
        [ value, value + span ]

    else if value > span - margin then
        [ value, value - span ]

    else
        [ value ]
