module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attr
import Random
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Svg.Events
import Time



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Cell =
    ( Int, Int )


type Status
    = Ready
    | Playing
    | Lost
    | Won


type alias Shock =
    { origin : Cell
    , elapsed : Float
    }


type alias Model =
    { mines : Set Cell
    , revealed : Set Cell
    , status : Status
    , pattern : List String
    , shock : Maybe Shock
    , time : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { mines = Set.empty
      , revealed = Set.empty
      , status = Ready
      , pattern = []
      , shock = Nothing
      , time = 0
      }
    , Random.generate PatternGenerated patternGenerator
    )



-- UPDATE


type Msg
    = Clicked Cell
    | MinesPlaced Cell (Set Cell)
    | PatternGenerated Pattern
    | Tick
    | Frame Float


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 1000 (\_ -> Tick)
        , Browser.Events.onAnimationFrameDelta Frame
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clicked cell ->
            case model.status of
                Ready ->
                    ( struck cell model, Random.generate (MinesPlaced cell) (mineGenerator cell) )

                Playing ->
                    ( reveal cell (struck cell model), Cmd.none )

                Lost ->
                    ( model, Cmd.none )

                Won ->
                    ( model, Cmd.none )

        MinesPlaced cell mines ->
            ( reveal cell { model | mines = mines, status = Playing }, Cmd.none )

        PatternGenerated pattern ->
            ( { model | pattern = patternRows pattern }, Cmd.none )

        Tick ->
            ( model, Random.generate PatternGenerated patternGenerator )

        Frame delta ->
            ( { model | time = model.time + delta, shock = advance delta model.shock }, Cmd.none )


struck : Cell -> Model -> Model
struck cell model =
    { model | shock = Just { origin = cell, elapsed = 0 } }


advance : Float -> Maybe Shock -> Maybe Shock
advance delta shock =
    case shock of
        Nothing ->
            Nothing

        Just current ->
            let
                elapsed =
                    current.elapsed + delta
            in
            if elapsed > shockLifetime then
                Nothing

            else
                Just { current | elapsed = elapsed }



-- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        [ patternLayer model.pattern
        , Html.div
            [ Attr.style "position" "relative"
            , Attr.style "display" "flex"
            , Attr.style "justify-content" "center"
            , Attr.style "align-items" "center"
            , Attr.style "min-height" "100vh"
            ]
            [ boardLayer model ]
        ]


boardLayer : Model -> Html Msg
boardLayer model =
    Html.div
        [ Attr.style "transition" "opacity 1.2s ease-out"
        , Attr.style "opacity"
            (if finished model.status then
                "0"

             else
                "1"
            )
        , Attr.style "pointer-events"
            (if finished model.status then
                "none"

             else
                "auto"
            )
        ]
        [ board model ]


finished : Status -> Bool
finished status =
    case status of
        Ready ->
            False

        Playing ->
            False

        Lost ->
            True

        Won ->
            True


patternLayer : List String -> Html msg
patternLayer rows =
    Html.pre
        [ Attr.style "position" "fixed"
        , Attr.style "inset" "0"
        , Attr.style "margin" "0"
        , Attr.style "overflow" "hidden"
        , Attr.style "font-family" "monospace"
        , Attr.style "font-size" "13px"
        , Attr.style "line-height" "1.2"
        , Attr.style "color" patternInk
        , Attr.style "pointer-events" "none"
        , Attr.style "user-select" "none"
        ]
        [ Html.text (String.join "\n" rows) ]


board : Model -> Svg Msg
board model =
    Svg.svg
        [ SvgAttr.width (String.fromFloat boardSize)
        , SvgAttr.height (String.fromFloat boardSize)
        , SvgAttr.viewBox ("0 0 " ++ String.fromFloat boardSize ++ " " ++ String.fromFloat boardSize)
        , SvgAttr.style "overflow: visible"
        ]
        (List.concatMap (cellView model) cells)


cellView : Model -> Cell -> List (Svg Msg)
cellView model cell =
    let
        ( column, row ) =
            cell

        ( dx, dy ) =
            offset model cell

        opened =
            Set.member cell model.revealed

        x =
            position column + gap / 2 + dx

        y =
            position row + gap / 2 + dy
    in
    Svg.rect
        [ SvgAttr.x (String.fromFloat x)
        , SvgAttr.y (String.fromFloat y)
        , SvgAttr.width (String.fromFloat cellSize)
        , SvgAttr.height (String.fromFloat cellSize)
        , SvgAttr.fill
            (if opened then
                ink

             else
                paper
            )
        , SvgAttr.stroke ink
        , SvgAttr.strokeWidth "2"
        , SvgAttr.shapeRendering "crispEdges"
        , SvgAttr.cursor "pointer"
        , Svg.Events.onClick (Clicked cell)
        ]
        []
        :: (if opened then
                labels model cell x y

            else
                []
           )


labels : Model -> Cell -> Float -> Float -> List (Svg msg)
labels model cell x y =
    case adjacentMines model.mines cell of
        0 ->
            []

        count ->
            [ label x y count ]


label : Float -> Float -> Int -> Svg msg
label x y count =
    Svg.text_
        [ SvgAttr.x (String.fromFloat (x + cellSize / 2))
        , SvgAttr.y (String.fromFloat (y + cellSize / 2))
        , SvgAttr.textAnchor "middle"
        , SvgAttr.dominantBaseline "central"
        , SvgAttr.fontFamily "monospace"
        , SvgAttr.fontSize (String.fromFloat (cellSize / 2))
        , SvgAttr.fill paper
        ]
        [ Svg.text (String.fromInt count) ]



-- RULES


mineCount : Int
mineCount =
    10


mineGenerator : Cell -> Random.Generator (Set Cell)
mineGenerator safe =
    let
        candidates =
            List.filter ((/=) safe) cells
    in
    Random.list (List.length candidates) (Random.float 0 1)
        |> Random.map
            (\keys ->
                List.map2 Tuple.pair keys candidates
                    |> List.sortBy Tuple.first
                    |> List.take mineCount
                    |> List.map Tuple.second
                    |> Set.fromList
            )


reveal : Cell -> Model -> Model
reveal cell model =
    if Set.member cell model.mines then
        { model | status = Lost }

    else
        let
            revealed =
                spread model.mines cell model.revealed
        in
        { model
            | revealed = revealed
            , status =
                if Set.size revealed + mineCount == List.length cells then
                    Won

                else
                    Playing
        }


spread : Set Cell -> Cell -> Set Cell -> Set Cell
spread mines cell revealed =
    if Set.member cell revealed then
        revealed

    else if adjacentMines mines cell == 0 then
        List.foldl (spread mines) (Set.insert cell revealed) (neighbors cell)

    else
        Set.insert cell revealed


adjacentMines : Set Cell -> Cell -> Int
adjacentMines mines cell =
    List.length (List.filter (\neighbor -> Set.member neighbor mines) (neighbors cell))


neighbors : Cell -> List Cell
neighbors ( column, row ) =
    List.concatMap
        (\columnOffset ->
            List.map (\rowOffset -> ( column + columnOffset, row + rowOffset )) [ -1, 0, 1 ]
        )
        [ -1, 0, 1 ]
        |> List.filter (\cell -> cell /= ( column, row ) && List.member cell cells)



-- LAYOUT


cellCount : Int
cellCount =
    8


spacing : Float
spacing =
    48


gap : Float
gap =
    6


cellSize : Float
cellSize =
    spacing - gap


margin : Float
margin =
    8


boardSize : Float
boardSize =
    spacing * toFloat cellCount + margin * 2


cells : List Cell
cells =
    let
        cellIndices =
            List.range 0 (cellCount - 1)
    in
    List.concatMap (\column -> List.map (Tuple.pair column) cellIndices) cellIndices


position : Int -> Float
position index =
    margin + spacing * toFloat index



-- COLOR


ink : String
ink =
    "#ababab"


paper : String
paper =
    "#ffffff"


patternInk : String
patternInk =
    "#eeeeee"



-- MOTION


offset : Model -> Cell -> ( Float, Float )
offset model cell =
    let
        ( shockX, shockY ) =
            displacement model.shock cell

        ( driftX, driftY ) =
            drift model.time cell
    in
    ( shockX + driftX, shockY + driftY )


shockAmplitude : Float
shockAmplitude =
    140


shockDuration : Float
shockDuration =
    700


shockDelay : Float
shockDelay =
    65


shockLifetime : Float
shockLifetime =
    shockDuration + shockDelay * maxDistance


maxDistance : Float
maxDistance =
    sqrt 2 * toFloat (cellCount - 1)


displacement : Maybe Shock -> Cell -> ( Float, Float )
displacement shock ( column, row ) =
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
                        shockAmplitude * sin (pi * local / shockDuration) * (distance / maxDistance) ^ 2
                in
                ( dx / distance * amplitude, dy / distance * amplitude )


driftAmplitude : Float
driftAmplitude =
    1.2


drift : Float -> Cell -> ( Float, Float )
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



-- PATTERN


type Pattern
    = Diagonals { phase : Float }
    | Waves { fx : Float, fy : Float, phase : Float }
    | Ripples { fx : Float, cx : Float, cy : Float }


patternWidth : Int
patternWidth =
    240


patternHeight : Int
patternHeight =
    100


patternGenerator : Random.Generator Pattern
patternGenerator =
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


patternRows : Pattern -> List String
patternRows pattern =
    List.map
        (\y -> String.concat (List.map (\x -> patternChar pattern x y) (List.range 0 (patternWidth - 1))))
        (List.range 0 (patternHeight - 1))


patternChar : Pattern -> Int -> Int -> String
patternChar pattern x y =
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
                    toFloat x - cx * toFloat patternWidth

                dy =
                    (toFloat y - cy * toFloat patternHeight) * 2
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
