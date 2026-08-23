module Motion exposing (Shock, advance, offset, shockLifetime)

import Geometry exposing (Vector)
import Millis exposing (Millis)



-- MOTION


type alias Shock =
    { origin : ( Int, Int )
    , elapsed : Millis
    }


advance : Millis -> Millis -> Maybe { a | elapsed : Millis } -> Maybe { a | elapsed : Millis }
advance lifetime delta timer =
    case timer of
        Nothing ->
            Nothing

        Just current ->
            let
                elapsed =
                    current.elapsed + delta
            in
            if elapsed > lifetime then
                Nothing

            else
                Just { current | elapsed = elapsed }


offset : Int -> Maybe Shock -> Millis -> ( Int, Int ) -> Vector
offset cellCount shock time cell =
    let
        ( shockX, shockY ) =
            displacement cellCount shock cell

        ( driftX, driftY ) =
            drift time cell
    in
    ( shockX + driftX, shockY + driftY )



-- SHOCK


shockAmplitude : Float
shockAmplitude =
    140


shockDuration : Millis
shockDuration =
    700


shockDelay : Millis
shockDelay =
    65


shockLifetime : Int -> Millis
shockLifetime cellCount =
    shockDuration + shockDelay * maxDistance cellCount


maxDistance : Int -> Float
maxDistance cellCount =
    sqrt 2 * toFloat (cellCount - 1)


displacement : Int -> Maybe Shock -> ( Int, Int ) -> Vector
displacement cellCount shock ( column, row ) =
    case shock of
        Nothing ->
            ( 0, 0 )

        Just { origin, elapsed } ->
            let
                ( originColumn, originRow ) =
                    origin

                dx =
                    toFloat (column - originColumn)

                dy =
                    toFloat (row - originRow)

                distance =
                    sqrt (dx * dx + dy * dy)

                local =
                    elapsed - distance * shockDelay
            in
            if distance == 0 || local <= 0 || local >= shockDuration then
                ( 0, 0 )

            else
                let
                    amplitude =
                        shockAmplitude * sin (pi * local / shockDuration) * (distance / maxDistance cellCount) ^ 2
                in
                ( dx / distance * amplitude, dy / distance * amplitude )



-- DRIFT


driftAmplitude : Float
driftAmplitude =
    1.2


drift : Millis -> ( Int, Int ) -> Vector
drift time ( column, row ) =
    let
        seconds =
            time / 1000

        seed =
            toFloat (column * 3 + row * 5)
    in
    ( wobble seconds seed 1.3 2.7
    , wobble seconds (seed * 1.7 + 2.1) 1.1 2.3
    )


wobble : Float -> Float -> Float -> Float -> Float
wobble seconds seed slow fast =
    driftAmplitude * (sin (seconds * slow + seed) + 0.5 * sin (seconds * fast + seed * 1.9))
