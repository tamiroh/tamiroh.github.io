module Main exposing (main)

import Boid exposing (Boid, Rect)
import Browser
import Browser.Dom
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Motion exposing (Pull, Shock)
import Pattern exposing (Pattern)
import Random
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Svg.Events
import Task
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


type alias Screen =
    { width : Float
    , height : Float
    }


type alias Model =
    { mines : Set Cell
    , revealed : Set Cell
    , status : Status
    , pattern : List String
    , shock : Maybe Shock
    , time : Float
    , lit : Bool
    , pull : Maybe Pull
    , screen : Screen
    , boids : List Boid
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { mines = Set.empty
      , revealed = Set.empty
      , status = Ready
      , pattern = []
      , shock = Nothing
      , time = 0
      , lit = True
      , pull = Nothing
      , screen = { width = 0, height = 0 }
      , boids = []
      }
    , Cmd.batch
        [ Random.generate PatternGenerated Pattern.generator
        , Task.perform GotViewport Browser.Dom.getViewport
        ]
    )



-- UPDATE


type Msg
    = Clicked Cell
    | MinesPlaced Cell (Set Cell)
    | PatternGenerated Pattern
    | Tick
    | Frame Float
    | Pulled
    | GotViewport Browser.Dom.Viewport
    | Resized Int Int
    | BoidsPlaced (List Boid)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 1000 (\_ -> Tick)
        , Browser.Events.onAnimationFrameDelta Frame
        , Browser.Events.onResize Resized
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
            ( { model | pattern = Pattern.toRows pattern }, Cmd.none )

        Tick ->
            ( model, Random.generate PatternGenerated Pattern.generator )

        Frame delta ->
            ( { model
                | time = model.time + delta
                , shock = Motion.advance (Motion.shockLifetime cellCount) delta model.shock
                , pull = Motion.advance Motion.pullDuration delta model.pull
                , boids = Boid.flock delta model.screen (obstacle model.screen) model.boids
              }
            , Cmd.none
            )

        Pulled ->
            ( { model | lit = not model.lit, pull = Just { elapsed = 0 } }, Cmd.none )

        GotViewport viewport ->
            let
                screen =
                    { width = viewport.viewport.width, height = viewport.viewport.height }
            in
            ( { model | screen = screen }, Random.generate BoidsPlaced (Boid.generator screen (obstacle screen)) )

        Resized width height ->
            ( { model | screen = { width = toFloat width, height = toFloat height } }, Cmd.none )

        BoidsPlaced boids ->
            ( { model | boids = boids }, Cmd.none )


struck : Cell -> Model -> Model
struck cell model =
    { model | shock = Just { origin = cell, elapsed = 0 } }



-- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        [ backgroundLayer model.lit
        , patternLayer model.lit model.pattern
        , boidLayer model.lit model.screen model.boids
        , Html.div
            [ Attr.style "position" "relative"
            , Attr.style "display" "flex"
            , Attr.style "justify-content" "center"
            , Attr.style "align-items" "center"
            , Attr.style "min-height" "100vh"
            ]
            [ boardLayer model ]
        , cordLayer model.lit (Motion.pullOffset model.pull)
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


backgroundLayer : Bool -> Html msg
backgroundLayer lit =
    Html.div
        [ Attr.style "position" "fixed"
        , Attr.style "inset" "0"
        , Attr.style "background-color" (paper lit)
        , Attr.style "pointer-events" "none"
        ]
        []


cordLayer : Bool -> Float -> Html Msg
cordLayer lit dy =
    Html.div
        [ Attr.style "position" "fixed"
        , Attr.style "top" "0"
        , Attr.style "right" (String.fromFloat cordInset ++ "px")
        , Attr.style "cursor" "pointer"
        , Attr.style "user-select" "none"
        , Html.Events.onClick Pulled
        ]
        [ cord lit dy ]


cord : Bool -> Float -> Svg msg
cord lit dy =
    Svg.svg
        [ SvgAttr.width (String.fromFloat cordWidth)
        , SvgAttr.height (String.fromFloat (cordLength + cordGripHeight + lineWidth))
        , SvgAttr.style "overflow: visible"
        ]
        [ Svg.line
            [ SvgAttr.x1 (String.fromFloat (cordWidth / 2))
            , SvgAttr.y1 "0"
            , SvgAttr.x2 (String.fromFloat (cordWidth / 2))
            , SvgAttr.y2 (String.fromFloat (cordLength + dy))
            , SvgAttr.stroke (ink lit)
            , SvgAttr.strokeWidth (String.fromFloat lineWidth)
            ]
            []
        , Svg.rect
            [ SvgAttr.x (String.fromFloat ((cordWidth - cordGripWidth) / 2))
            , SvgAttr.y (String.fromFloat (cordLength + dy))
            , SvgAttr.width (String.fromFloat cordGripWidth)
            , SvgAttr.height (String.fromFloat cordGripHeight)
            , SvgAttr.rx (String.fromFloat (cordGripWidth / 2))
            , SvgAttr.fill (paper lit)
            , SvgAttr.stroke (ink lit)
            , SvgAttr.strokeWidth (String.fromFloat lineWidth)
            ]
            []
        ]


boidLayer : Bool -> Screen -> List Boid -> Html msg
boidLayer lit screen boids =
    Svg.svg
        [ SvgAttr.width (String.fromFloat screen.width)
        , SvgAttr.height (String.fromFloat screen.height)
        , Attr.style "position" "fixed"
        , Attr.style "inset" "0"
        , Attr.style "pointer-events" "none"
        ]
        (List.concatMap (boidView lit screen) boids)


boidView : Bool -> Screen -> Boid -> List (Svg msg)
boidView lit screen boid =
    List.map (dot lit) (Boid.wrapCopies screen boid)


dot : Bool -> ( Float, Float ) -> Svg msg
dot lit ( x, y ) =
    Svg.circle
        [ SvgAttr.cx (String.fromFloat x)
        , SvgAttr.cy (String.fromFloat y)
        , SvgAttr.r (String.fromFloat Boid.radius)
        , SvgAttr.fill (paper lit)
        , SvgAttr.stroke (ink lit)
        , SvgAttr.strokeWidth (String.fromFloat lineWidth)
        ]
        []


patternLayer : Bool -> List String -> Html msg
patternLayer lit rows =
    Html.pre
        [ Attr.style "position" "fixed"
        , Attr.style "inset" "0"
        , Attr.style "margin" "0"
        , Attr.style "overflow" "hidden"
        , Attr.style "font-family" "monospace"
        , Attr.style "font-size" "13px"
        , Attr.style "line-height" "1.2"
        , Attr.style "color" (patternInk lit)
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
            Motion.offset cellCount model.shock model.time cell

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
                ink model.lit

             else
                paper model.lit
            )
        , SvgAttr.stroke (ink model.lit)
        , SvgAttr.strokeWidth (String.fromFloat lineWidth)
        , SvgAttr.shapeRendering "crispEdges"
        , SvgAttr.cursor "pointer"
        , Svg.Events.onClick (Clicked cell)
        ]
        []
        :: (if opened then
                pips model cell x y

            else
                []
           )


pips : Model -> Cell -> Float -> Float -> List (Svg msg)
pips model cell x y =
    List.map (pip model.lit x y) (pipCells (adjacentMines model.mines cell))


pip : Bool -> Float -> Float -> ( Int, Int ) -> Svg msg
pip lit x y ( column, row ) =
    Svg.circle
        [ SvgAttr.cx (String.fromFloat (x + pipOffset column))
        , SvgAttr.cy (String.fromFloat (y + pipOffset row))
        , SvgAttr.r (String.fromFloat pipRadius)
        , SvgAttr.fill (paper lit)
        ]
        []



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
    9


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


obstacle : Screen -> Rect
obstacle screen =
    { left = screen.width / 2 - boardSize / 2
    , top = screen.height / 2 - boardSize / 2
    , right = screen.width / 2 + boardSize / 2
    , bottom = screen.height / 2 + boardSize / 2
    }


position : Int -> Float
position index =
    margin + spacing * toFloat index


lineWidth : Float
lineWidth =
    2


cordInset : Float
cordInset =
    64


cordLength : Float
cordLength =
    96


cordWidth : Float
cordWidth =
    16


cordGripWidth : Float
cordGripWidth =
    9


cordGripHeight : Float
cordGripHeight =
    16


pipRadius : Float
pipRadius =
    cellSize / 14


pipOffset : Int -> Float
pipOffset index =
    cellSize * (0.25 + 0.25 * toFloat index)


pipCells : Int -> List ( Int, Int )
pipCells count =
    let
        corners =
            [ ( 0, 0 ), ( 2, 0 ), ( 0, 2 ), ( 2, 2 ) ]

        sides =
            [ ( 0, 1 ), ( 2, 1 ) ]

        center =
            [ ( 1, 1 ) ]
    in
    case count of
        1 ->
            center

        2 ->
            [ ( 0, 0 ), ( 2, 2 ) ]

        3 ->
            [ ( 0, 0 ), ( 1, 1 ), ( 2, 2 ) ]

        4 ->
            corners

        5 ->
            corners ++ center

        6 ->
            corners ++ sides

        7 ->
            corners ++ sides ++ center

        8 ->
            corners ++ sides ++ [ ( 1, 0 ), ( 1, 2 ) ]

        _ ->
            []



-- COLOR


ink : Bool -> String
ink lit =
    if lit then
        "#ababab"

    else
        "#545454"


paper : Bool -> String
paper lit =
    if lit then
        "#ffffff"

    else
        "#000000"


patternInk : Bool -> String
patternInk lit =
    if lit then
        "#eeeeee"

    else
        "#111111"
