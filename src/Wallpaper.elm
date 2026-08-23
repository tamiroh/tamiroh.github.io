module Wallpaper exposing (Wallpaper, blank, generator, view)

import Html exposing (Html)
import Html.Attributes as Attr
import Random
import Screen exposing (Screen)



-- WALLPAPER


type Wallpaper
    = Wallpaper String


blank : Wallpaper
blank =
    Wallpaper ""


generator : Screen -> Random.Generator Wallpaper
generator screen =
    Random.map (\chosen -> Wallpaper (render (columns screen) (rows screen) chosen)) design


view : String -> Wallpaper -> Html msg
view ink (Wallpaper text) =
    Html.pre
        [ Attr.style "position" "fixed"
        , Attr.style "inset" "0"
        , Attr.style "margin" "0"
        , Attr.style "overflow" "hidden"
        , Attr.style "font-family" "monospace"
        , Attr.style "font-size" (String.fromFloat fontSize ++ "px")
        , Attr.style "line-height" (String.fromFloat lineHeight)
        , Attr.style "color" ink
        , Attr.style "pointer-events" "none"
        , Attr.style "user-select" "none"
        ]
        [ Html.text text ]



-- METRICS


fontSize : Float
fontSize =
    13


lineHeight : Float
lineHeight =
    1.2


charWidth : Float
charWidth =
    fontSize * 0.45


columns : Screen -> Int
columns screen =
    ceiling (screen.width / charWidth) + 1


rows : Screen -> Int
rows screen =
    ceiling (screen.height / (fontSize * lineHeight)) + 1



-- DESIGN


type Design
    = Diagonals { phase : Float }
    | Waves { fx : Float, fy : Float, phase : Float }
    | Ripples { fx : Float, cx : Float, cy : Float }



-- GENERATE


design : Random.Generator Design
design =
    Random.uniform diagonalsGenerator [ wavesGenerator, ripplesGenerator ]
        |> Random.andThen identity


diagonalsGenerator : Random.Generator Design
diagonalsGenerator =
    Random.map (\phase -> Diagonals { phase = phase }) angle


wavesGenerator : Random.Generator Design
wavesGenerator =
    Random.map3 (\fx fy phase -> Waves { fx = fx, fy = fy, phase = phase })
        frequency
        frequency
        angle


ripplesGenerator : Random.Generator Design
ripplesGenerator =
    Random.map3 (\fx cx cy -> Ripples { fx = fx, cx = cx, cy = cy })
        frequency
        (Random.float 0 1)
        (Random.float 0 1)


frequency : Random.Generator Float
frequency =
    Random.float 0.1 0.6


angle : Random.Generator Float
angle =
    Random.float 0 (2 * pi)



-- RENDER


render : Int -> Int -> Design -> String
render wide tall chosen =
    List.range 0 (tall - 1)
        |> List.map
            (\y -> String.concat (List.map (\x -> charAt wide tall chosen x y) (List.range 0 (wide - 1))))
        |> String.join "\n"


charAt : Int -> Int -> Design -> Int -> Int -> String
charAt wide tall chosen x y =
    case chosen of
        Diagonals { phase } ->
            if noise phase x y < 0.5 then
                "/"

            else
                "\\"

        Waves { fx, fy, phase } ->
            ramp (sin (toFloat x * fx + phase) + sin (toFloat y * 2 * fy))

        Ripples { fx, cx, cy } ->
            let
                dx =
                    toFloat x - cx * toFloat wide

                dy =
                    (toFloat y - cy * toFloat tall) * 2
            in
            ramp (2 * sin (sqrt (dx * dx + dy * dy) * fx))


ramp : Float -> String
ramp value =
    let
        index =
            clamp 0 9 (floor ((value + 2) / 4 * 9))
    in
    String.slice index (index + 1) " .:-=+*#%@"


noise : Float -> Int -> Int -> Float
noise phase x y =
    let
        value =
            sin (toFloat x * 12.9898 + toFloat y * 78.233 + phase) * 43758.5453
    in
    value - toFloat (floor value)
