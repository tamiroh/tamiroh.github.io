module Wobble exposing (at)

import Geometry exposing (Vector)
import Millis exposing (Millis)



-- WOBBLE


amplitude : Float
amplitude =
    1.2


at : Millis -> Float -> Vector
at time seed =
    let
        seconds =
            time / 1000
    in
    ( wave seconds seed 1.3 2.7
    , wave seconds (seed * 1.7 + 2.1) 1.1 2.3
    )


wave : Float -> Float -> Float -> Float -> Float
wave seconds seed slow fast =
    amplitude * (sin (seconds * slow + seed) + 0.5 * sin (seconds * fast + seed * 1.9))
