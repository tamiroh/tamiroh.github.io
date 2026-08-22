module Motion exposing (Pull, Shock, advance, offset, pullDuration, pullOffset, shockLifetime)

-- MOTION


type alias Shock =
    { origin : ( Int, Int )
    , elapsed : Float
    }


type alias Pull =
    { elapsed : Float }


advance : Float -> Float -> Maybe { a | elapsed : Float } -> Maybe { a | elapsed : Float }
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


offset : Int -> Maybe Shock -> Float -> ( Int, Int ) -> ( Float, Float )
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


shockDuration : Float
shockDuration =
    700


shockDelay : Float
shockDelay =
    65


shockLifetime : Int -> Float
shockLifetime cellCount =
    shockDuration + shockDelay * maxDistance cellCount


maxDistance : Int -> Float
maxDistance cellCount =
    sqrt 2 * toFloat (cellCount - 1)


displacement : Int -> Maybe Shock -> ( Int, Int ) -> ( Float, Float )
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



-- PULL


pullAmplitude : Float
pullAmplitude =
    20


pullDuration : Float
pullDuration =
    600


pullCycles : Float
pullCycles =
    1.25


pullDamping : Float
pullDamping =
    4


pullOffset : Maybe Pull -> Float
pullOffset pull =
    case pull of
        Nothing ->
            0

        Just { elapsed } ->
            let
                phase =
                    elapsed / pullDuration
            in
            pullAmplitude
                * sin (pullCycles * 2 * pi * phase)
                * e
                ^ negate (pullDamping * phase)



-- DRIFT


driftAmplitude : Float
driftAmplitude =
    1.2


drift : Float -> ( Int, Int ) -> ( Float, Float )
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
