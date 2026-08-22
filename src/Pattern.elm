module Pattern exposing (Pattern, generator, toText)

import Random



-- PATTERN


type Pattern
    = Diagonals { phase : Float }
    | Waves { fx : Float, fy : Float, phase : Float }
    | Ripples { fx : Float, cx : Float, cy : Float }



-- GENERATE


generator : Random.Generator Pattern
generator =
    Random.uniform diagonalsGenerator [ wavesGenerator, ripplesGenerator ]
        |> Random.andThen identity


diagonalsGenerator : Random.Generator Pattern
diagonalsGenerator =
    Random.map (\phase -> Diagonals { phase = phase }) angle


wavesGenerator : Random.Generator Pattern
wavesGenerator =
    Random.map3 (\fx fy phase -> Waves { fx = fx, fy = fy, phase = phase })
        frequency
        frequency
        angle


ripplesGenerator : Random.Generator Pattern
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


toText : Int -> Int -> Pattern -> String
toText columns rows pattern =
    List.range 0 (rows - 1)
        |> List.map
            (\y -> String.concat (List.map (\x -> charAt columns rows pattern x y) (List.range 0 (columns - 1))))
        |> String.join "\n"


charAt : Int -> Int -> Pattern -> Int -> Int -> String
charAt columns rows pattern x y =
    case pattern of
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
                    toFloat x - cx * toFloat columns

                dy =
                    (toFloat y - cy * toFloat rows) * 2
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
