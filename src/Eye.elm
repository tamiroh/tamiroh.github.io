module Eye exposing (Eye, generator, step, view)

import Field exposing (Field)
import Geometry exposing (Position)
import Millis exposing (Millis)
import Random
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Transform



-- EYE


type alias Look =
    { ink : String
    , paper : String
    , stroke : Float
    }


type alias Eye =
    { x : Float
    , y : Float
    , born : Millis
    , life : Millis
    , shut : Maybe Float
    }


span : Float
span =
    26


opening : Float
opening =
    18



-- SPAWN


chance : Float
chance =
    0.4


tries : Int
tries =
    6


generator : Field -> Millis -> Random.Generator (Maybe Eye)
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
                    (Field.spot (span / 2) field tries)
                    (Random.float 2400 4600)
        )
        (Random.float 0 1)



-- LIFE


shy : Float
shy =
    90


blink : Millis
blink =
    110


step : Millis -> Maybe Position -> Eye -> Maybe Eye
step now pointer eye =
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


closing : Millis -> Eye -> Float
closing now eye =
    case eye.shut of
        Nothing ->
            1

        Just at ->
            max 0 (1 - (now - at) / blink)



-- VIEW


shut : Float
shut =
    0.04


snap : Float
snap =
    5


aperture : Millis -> Eye -> Float
aperture now eye =
    min 1 (snap * sin (pi * clamp 0 1 ((now - eye.born) / eye.life)))
        * closing now eye


view : Look -> Millis -> Eye -> Maybe (Svg msg)
view look now eye =
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
                    (Transform.translate ( eye.x, eye.y ))
                , SvgAttr.stroke look.ink
                , SvgAttr.strokeWidth (String.fromFloat look.stroke)
                , SvgAttr.strokeLinejoin "round"
                ]
                [ Svg.path [ SvgAttr.d (lens half spread), SvgAttr.fill look.paper ] []
                , Svg.circle
                    [ SvgAttr.r (String.fromFloat (min (spread * 0.55) (half * 0.4)))
                    , SvgAttr.fill look.ink
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
