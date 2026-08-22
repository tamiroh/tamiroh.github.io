module Main exposing (main)

import Boid exposing (Boid)
import Browser
import Browser.Dom
import Browser.Events
import Geometry exposing (Position, Rect, Screen)
import Grid exposing (Cell)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Json.Decode
import Minesweeper
import Motion exposing (Pull, Shock)
import Othello
import Pattern exposing (Pattern)
import Process
import Random
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


type alias Model =
    { play : Play
    , pattern : List String
    , shock : Maybe Shock
    , time : Float
    , lit : Bool
    , pull : Maybe Pull
    , screen : Screen
    , boids : List Boid
    , pointer : Maybe Position
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { play = Fresh
      , pattern = []
      , shock = Nothing
      , time = 0
      , lit = True
      , pull = Nothing
      , screen = { width = 0, height = 0 }
      , boids = []
      , pointer = Nothing
      }
    , Cmd.batch
        [ Random.generate PatternGenerated Pattern.generator
        , Task.perform GotViewport Browser.Dom.getViewport
        ]
    )



-- UPDATE


type Play
    = Fresh
    | Mines Minesweeper.Game
    | Discs Othello.Board


type Msg
    = Clicked Cell
    | Started Play
    | Answered
    | PatternGenerated Pattern
    | Tick
    | Frame Float
    | Pulled
    | GotViewport Browser.Dom.Viewport
    | Resized Int Int
    | BoidsPlaced (List Boid)
    | PointerMoved Position


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 1000 (\_ -> Tick)
        , Browser.Events.onAnimationFrameDelta Frame
        , Browser.Events.onResize Resized
        , Browser.Events.onMouseMove pointerDecoder
        ]


pointerDecoder : Json.Decode.Decoder Msg
pointerDecoder =
    Json.Decode.map2 (\x y -> PointerMoved ( x, y ))
        (Json.Decode.field "clientX" Json.Decode.float)
        (Json.Decode.field "clientY" Json.Decode.float)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clicked cell ->
            case model.play of
                Fresh ->
                    ( struck cell model, Random.generate Started (startGenerator cell) )

                Mines game ->
                    if Minesweeper.finished game then
                        ( model, Cmd.none )

                    else
                        ( struck cell { model | play = Mines (Minesweeper.reveal cell game) }, Cmd.none )

                Discs othello ->
                    case Othello.play cell othello of
                        Nothing ->
                            ( model, Cmd.none )

                        Just next ->
                            ( struck cell { model | play = Discs next }, think next )

        Started play ->
            ( { model | play = play }, Cmd.none )

        Answered ->
            case model.play of
                Discs othello ->
                    let
                        next =
                            Othello.respond othello
                    in
                    ( { model | play = Discs next }, think next )

                _ ->
                    ( model, Cmd.none )

        PatternGenerated pattern ->
            ( { model | pattern = Pattern.toRows (patternColumns model.screen) (patternRows model.screen) pattern }, Cmd.none )

        Tick ->
            ( model, Random.generate PatternGenerated Pattern.generator )

        Frame delta ->
            ( { model
                | time = model.time + delta
                , shock = Motion.advance (Motion.shockLifetime Grid.count) delta model.shock
                , pull = Motion.advance Motion.pullDuration delta model.pull
                , boids = Boid.flock delta model.screen (obstacle model.screen) model.pointer model.boids
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
            ( { model | screen = screen }
            , Cmd.batch
                [ Random.generate BoidsPlaced (Boid.generator screen (obstacle screen))
                , Random.generate PatternGenerated Pattern.generator
                ]
            )

        Resized width height ->
            ( { model | screen = { width = toFloat width, height = toFloat height } }, Cmd.none )

        BoidsPlaced boids ->
            ( { model | boids = boids }, Cmd.none )

        PointerMoved point ->
            ( { model | pointer = Just point }, Cmd.none )


startGenerator : Cell -> Random.Generator Play
startGenerator cell =
    Random.uniform
        (Random.map (\game -> Mines (Minesweeper.reveal cell game)) (Minesweeper.start cell))
        [ Random.constant (Discs Othello.new) ]
        |> Random.andThen identity


think : Othello.Board -> Cmd Msg
think othello =
    if Othello.thinking othello then
        Task.perform (\_ -> Answered) (Process.sleep thinkingDelay)

    else
        Cmd.none


over : Play -> Bool
over play =
    case play of
        Fresh ->
            False

        Mines game ->
            Minesweeper.finished game

        Discs othello ->
            Othello.isOver othello


struck : Cell -> Model -> Model
struck cell model =
    { model | shock = Just { origin = cell, elapsed = 0 } }



-- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        [ pageStyle
        , backgroundLayer model.lit
        , patternLayer model.lit model.pattern
        , boidLayer model.lit model.screen model.boids
        , Html.div
            [ Attr.style "position" "fixed"
            , Attr.style "inset" "0"
            , Attr.style "display" "flex"
            , Attr.style "justify-content" "center"
            , Attr.style "align-items" "center"
            ]
            [ boardLayer model ]
        , cordLayer model.lit (Motion.pullOffset model.pull)
        ]


boardLayer : Model -> Html Msg
boardLayer model =
    Html.div
        [ Attr.style "pointer-events"
            (if over model.play then
                "none"

             else
                "auto"
            )
        ]
        [ board model ]


pageStyle : Html msg
pageStyle =
    Html.node "style"
        []
        [ Html.text "html,body{margin:0;overflow:hidden;overscroll-behavior:none}" ]


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


dot : Bool -> Position -> Svg msg
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


skull : Bool -> Float -> Float -> List (Svg msg)
skull lit x y =
    let
        midX =
            x + cellSize / 2

        midY =
            y + cellSize / 2
    in
    [ Svg.circle
        [ SvgAttr.cx (String.fromFloat midX)
        , SvgAttr.cy (String.fromFloat (midY - skullLift))
        , SvgAttr.r (String.fromFloat skullRadius)
        , SvgAttr.fill (paper lit)
        ]
        []
    , Svg.rect
        [ SvgAttr.x (String.fromFloat (midX - skullJawWidth / 2))
        , SvgAttr.y (String.fromFloat (midY + skullJawTop))
        , SvgAttr.width (String.fromFloat skullJawWidth)
        , SvgAttr.height (String.fromFloat skullJawHeight)
        , SvgAttr.rx (String.fromFloat (skullJawHeight / 3))
        , SvgAttr.fill (paper lit)
        ]
        []
    , socket lit (midX - skullEyeGap) midY
    , socket lit (midX + skullEyeGap) midY
    , tooth lit (midX - skullToothGap) midY
    , tooth lit (midX + skullToothGap) midY
    ]


socket : Bool -> Float -> Float -> Svg msg
socket lit midX midY =
    Svg.circle
        [ SvgAttr.cx (String.fromFloat midX)
        , SvgAttr.cy (String.fromFloat (midY - skullEyeLift))
        , SvgAttr.r (String.fromFloat skullEyeRadius)
        , SvgAttr.fill (ink lit)
        ]
        []


tooth : Bool -> Float -> Float -> Svg msg
tooth lit midX midY =
    Svg.rect
        [ SvgAttr.x (String.fromFloat (midX - skullToothWidth / 2))
        , SvgAttr.y (String.fromFloat (midY + skullToothTop))
        , SvgAttr.width (String.fromFloat skullToothWidth)
        , SvgAttr.height (String.fromFloat skullToothHeight)
        , SvgAttr.fill (ink lit)
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
        , Attr.style "font-size" (String.fromFloat patternFontSize ++ "px")
        , Attr.style "line-height" (String.fromFloat patternLineHeight)
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
        (List.concatMap (cellView model) Grid.cells)


cellView : Model -> Cell -> List (Svg Msg)
cellView model cell =
    let
        ( column, row ) =
            cell

        ( dx, dy ) =
            Motion.offset Grid.count model.shock model.time cell

        opened =
            case model.play of
                Mines game ->
                    Minesweeper.faceOf cell game /= Minesweeper.Hidden

                _ ->
                    False

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
        :: cellMarks model cell x y


cellMarks : Model -> Cell -> Float -> Float -> List (Svg msg)
cellMarks model cell x y =
    case model.play of
        Fresh ->
            []

        Mines game ->
            marks model.lit (Minesweeper.faceOf cell game) x y

        Discs othello ->
            case Othello.discAt cell othello of
                Nothing ->
                    []

                Just side ->
                    [ disc model.lit side x y ]


disc : Bool -> Othello.Disc -> Float -> Float -> Svg msg
disc lit side x y =
    Svg.circle
        [ SvgAttr.cx (String.fromFloat (x + cellSize / 2))
        , SvgAttr.cy (String.fromFloat (y + cellSize / 2))
        , SvgAttr.r (String.fromFloat discRadius)
        , SvgAttr.fill
            (case side of
                Othello.Black ->
                    ink lit

                Othello.White ->
                    paper lit
            )
        , SvgAttr.stroke (ink lit)
        , SvgAttr.strokeWidth (String.fromFloat lineWidth)
        ]
        []


marks : Bool -> Minesweeper.Face -> Float -> Float -> List (Svg msg)
marks lit face x y =
    case face of
        Minesweeper.Hidden ->
            []

        Minesweeper.Blank ->
            []

        Minesweeper.Count count ->
            List.map (pip lit x y) (pipCells count)

        Minesweeper.Mine ->
            skull lit x y


pip : Bool -> Float -> Float -> ( Int, Int ) -> Svg msg
pip lit x y ( column, row ) =
    Svg.circle
        [ SvgAttr.cx (String.fromFloat (x + pipOffset column))
        , SvgAttr.cy (String.fromFloat (y + pipOffset row))
        , SvgAttr.r (String.fromFloat pipRadius)
        , SvgAttr.fill (paper lit)
        ]
        []



-- LAYOUT


lineWidth : Float
lineWidth =
    2


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
    spacing * toFloat Grid.count + margin * 2


position : Int -> Float
position index =
    margin + spacing * toFloat index


obstacle : Screen -> Rect
obstacle screen =
    { left = screen.width / 2 - boardSize / 2
    , top = screen.height / 2 - boardSize / 2
    , right = screen.width / 2 + boardSize / 2
    , bottom = screen.height / 2 + boardSize / 2
    }



-- CORD


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



-- PIPS


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



-- DISCS


discRadius : Float
discRadius =
    cellSize * 0.34


thinkingDelay : Float
thinkingDelay =
    420



-- SKULL


skullRadius : Float
skullRadius =
    cellSize * 0.26


skullLift : Float
skullLift =
    cellSize * 0.06


skullJawWidth : Float
skullJawWidth =
    cellSize * 0.32


skullJawHeight : Float
skullJawHeight =
    cellSize * 0.17


skullJawTop : Float
skullJawTop =
    cellSize * 0.1


skullEyeRadius : Float
skullEyeRadius =
    cellSize * 0.095


skullEyeGap : Float
skullEyeGap =
    cellSize * 0.105


skullEyeLift : Float
skullEyeLift =
    cellSize * 0.07


skullToothWidth : Float
skullToothWidth =
    cellSize * 0.044


skullToothHeight : Float
skullToothHeight =
    cellSize * 0.09


skullToothTop : Float
skullToothTop =
    cellSize * 0.13


skullToothGap : Float
skullToothGap =
    cellSize * 0.055



-- BACKDROP


patternFontSize : Float
patternFontSize =
    13


patternLineHeight : Float
patternLineHeight =
    1.2


patternCharWidth : Float
patternCharWidth =
    patternFontSize * 0.45


patternColumns : Screen -> Int
patternColumns screen =
    ceiling (screen.width / patternCharWidth) + 1


patternRows : Screen -> Int
patternRows screen =
    ceiling (screen.height / (patternFontSize * patternLineHeight)) + 1



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
