module Eye exposing (Eye, alive, generator, view)

import Field exposing (Field)
import Geometry exposing (Position)
import Random
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr



-- EYE


type alias Eye =
    { x : Float
    , y : Float
    , born : Float
    , life : Float
    , shut : Maybe Float
    }


span : Float
span =
    26


opening : Float
opening =
    18


shut : Float
shut =
    0.04


snap : Float
snap =
    5


chance : Float
chance =
    0.4


shy : Float
shy =
    90


blink : Float
blink =
    110


tries : Int
tries =
    6


generator : Field -> Float -> Random.Generator (Maybe Eye)
generator field now =
    Random.andThen
        (\roll ->
            if roll > chance then
                Random.constant Nothing

            else
                Random.map2
                    (\place life ->
                        Maybe.map
                            (\( x, y ) -> { x = x, y = y, born = now, life = life, shut = Nothing })
                            place
                    )
                    (spot field tries)
                    (Random.float 2400 4600)
        )
        (Random.float 0 1)


spot : Field -> Int -> Random.Generator (Maybe Position)
spot field attempts =
    Random.map2 Tuple.pair (Random.float 0 field.screen.width) (Random.float 0 field.screen.height)
        |> Random.andThen
            (\point ->
                if Field.fits (span / 2) field point then
                    Random.constant (Just point)

                else if attempts <= 0 then
                    Random.constant Nothing

                else
                    spot field (attempts - 1)
            )


alive : Float -> Maybe Position -> Eye -> Maybe Eye
alive now pointer eye =
    if now - eye.born >= eye.life then
        Nothing

    else
        case eye.shut of
            Just at ->
                if now - at >= blink then
                    Nothing

                else
                    Just eye

            Nothing ->
                if noticed pointer eye then
                    Just { eye | shut = Just now }

                else
                    Just eye


noticed : Maybe Position -> Eye -> Bool
noticed pointer eye =
    case pointer of
        Nothing ->
            False

        Just ( px, py ) ->
            (px - eye.x) ^ 2 + (py - eye.y) ^ 2 < shy ^ 2


aperture : Float -> Eye -> Float
aperture now eye =
    min 1 (snap * sin (pi * clamp 0 1 ((now - eye.born) / eye.life)))
        * closing now eye


closing : Float -> Eye -> Float
closing now eye =
    case eye.shut of
        Nothing ->
            1

        Just at ->
            max 0 (1 - (now - at) / blink)


view : String -> String -> Float -> Float -> Eye -> Maybe (Svg msg)
view ink paper stroke now eye =
    let
        open =
            aperture now eye
    in
    if open < shut then
        Nothing

    else
        let
            half =
                span / 2

            spread =
                opening / 2 * open
        in
        Just
            (Svg.g
                [ SvgAttr.transform
                    ("translate(" ++ String.fromFloat eye.x ++ "," ++ String.fromFloat eye.y ++ ")")
                , SvgAttr.stroke ink
                , SvgAttr.strokeWidth (String.fromFloat stroke)
                , SvgAttr.strokeLinejoin "round"
                ]
                [ Svg.path [ SvgAttr.d (lens half spread), SvgAttr.fill paper ] []
                , Svg.circle
                    [ SvgAttr.r (String.fromFloat (min (spread * 0.55) (half * 0.4)))
                    , SvgAttr.fill ink
                    , SvgAttr.stroke "none"
                    ]
                    []
                ]
            )


lens : Float -> Float -> String
lens half spread =
    String.join " "
        [ "M"
        , String.fromFloat (negate half)
        , "0 Q 0"
        , String.fromFloat spread
        , String.fromFloat half
        , "0 Q 0"
        , String.fromFloat (negate spread)
        , String.fromFloat (negate half)
        , "0 Z"
        ]
