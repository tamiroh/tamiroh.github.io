module Main exposing (main)

import Bird
import Boid exposing (Boid)
import Browser
import Browser.Dom
import Browser.Events
import Cord
import Cursor
import Eye exposing (Eye)
import Field exposing (Field)
import Geometry exposing (Position, Screen, Vector, wrap, wrapDelta)
import Grid exposing (Cell)
import Html exposing (Html)
import Html.Attributes as Attr
import Json.Decode
import Minesweeper
import Motion exposing (Pull, Shock)
import Othello
import Pattern exposing (Pattern)
import Process
import Random
import Skull
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Svg.Events
import Task
import Time
import Walker



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
    , pattern : String
    , shock : Maybe Shock
    , time : Float
    , theme : Theme
    , pull : Maybe Pull
    , screen : Screen
    , boids : List Boid
    , pointer : Maybe Position
    , walked : Float
    , eyes : List Eye
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { play = Fresh
      , pattern = ""
      , shock = Nothing
      , time = 0
      , theme = Light
      , pull = Nothing
      , screen = { width = 0, height = 0 }
      , boids = []
      , pointer = Nothing
      , walked = 0
      , eyes = []
      }
    , Cmd.batch
        [ Random.generate PatternGenerated Pattern.generator
        , Task.perform GotViewport Browser.Dom.getViewport
        ]
    )



-- UPDATE


type Content
    = Bare
    | Face Minesweeper.Face
    | Stone Othello.Disc


type Play
    = Fresh
    | Mines Minesweeper.Game
    | Discs Othello.Board


type Msg
    = CellClicked Cell
    | CordPulled
    | GameStarted Play
    | OthelloResponded
    | AnimationFramePassed Float
    | SecondPassed
    | PointerMoved Position
    | GotViewport Browser.Dom.Viewport
    | WindowResized Int Int
    | PatternGenerated Pattern
    | BoidsPlaced (List Boid)
    | EyeOpened (Maybe Eye)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 1000 (\_ -> SecondPassed)
        , Browser.Events.onAnimationFrameDelta AnimationFramePassed
        , Browser.Events.onResize WindowResized
        , Browser.Events.onMouseMove pointerDecoder
        ]


pointerDecoder : Json.Decode.Decoder Msg
pointerDecoder =
    Json.Decode.map PointerMoved positionDecoder


positionDecoder : Json.Decode.Decoder Position
positionDecoder =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.field "clientX" Json.Decode.float)
        (Json.Decode.field "clientY" Json.Decode.float)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- PLAYER
        CellClicked cell ->
            case model.play of
                Fresh ->
                    ( struck cell model, Random.generate GameStarted (startGenerator cell) )

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

        CordPulled ->
            ( { model | theme = toggle model.theme, pull = Just { elapsed = 0 } }, Cmd.none )

        -- GAME
        GameStarted play ->
            ( { model | play = play }, Cmd.none )

        OthelloResponded ->
            case model.play of
                Discs othello ->
                    let
                        next =
                            Othello.respond othello
                    in
                    ( { model | play = Discs next }, think next )

                _ ->
                    ( model, Cmd.none )

        -- CLOCK
        AnimationFramePassed delta ->
            ( { model
                | time = model.time + delta
                , shock = Motion.advance (Motion.shockLifetime Grid.count) delta model.shock
                , pull = Motion.advance Motion.pullDuration delta model.pull
                , boids = Boid.flock delta (field model.screen) model.pointer model.boids
                , walked = model.walked + pace model * delta / 1000
                , eyes = List.filterMap (Eye.alive model.time model.pointer) model.eyes
              }
            , Cmd.none
            )

        SecondPassed ->
            ( model
            , Cmd.batch
                [ Random.generate PatternGenerated Pattern.generator
                , Random.generate EyeOpened (Eye.generator (field model.screen) model.time)
                ]
            )

        -- ENVIRONMENT
        PointerMoved point ->
            ( { model | pointer = Just point }, Cmd.none )

        GotViewport viewport ->
            let
                screen =
                    { width = viewport.viewport.width, height = viewport.viewport.height }
            in
            ( { model | screen = screen }
            , Cmd.batch
                [ Random.generate BoidsPlaced (Boid.generator (field screen))
                , Random.generate PatternGenerated Pattern.generator
                ]
            )

        WindowResized width height ->
            ( { model | screen = { width = toFloat width, height = toFloat height } }, Cmd.none )

        -- RANDOM
        PatternGenerated pattern ->
            ( { model | pattern = Pattern.toText (patternColumns model.screen) (patternRows model.screen) pattern }, Cmd.none )

        BoidsPlaced boids ->
            ( { model | boids = boids }, Cmd.none )

        EyeOpened opened ->
            case opened of
                Nothing ->
                    ( model, Cmd.none )

                Just eye ->
                    ( { model | eyes = eye :: model.eyes }, Cmd.none )


pace : Model -> Float
pace model =
    case model.pointer of
        Nothing ->
            walkerSpeed

        Just ( px, py ) ->
            let
                aside =
                    wrapDelta model.screen.width (strolled model.screen model.walked - px)

                above =
                    ground model.screen - Walker.height / 2 - py

                span =
                    sqrt (aside * aside + above * above)

                near =
                    max 0 (1 - span / walkerSense)

                lean =
                    negate (clamp -1 1 (aside / walkerFocus))

                panic =
                    walkerPanic
                        * max 0 ((walkerPanicRange / max walkerPanicFloor span) ^ 2 - 1)
            in
            min (walkerSpeed * walkerLimit)
                (max 0
                    (walkerSpeed
                        * (1 - near * walkerBrake * max 0 (negate lean) + near * walkerRush * max 0 lean)
                    )
                    + walkerSpeed
                    * panic
                    * max 0 lean
                )


startGenerator : Cell -> Random.Generator Play
startGenerator cell =
    Random.uniform
        (Random.map (\game -> Mines (Minesweeper.reveal cell game)) (Minesweeper.start cell))
        [ Random.constant (Discs Othello.new) ]
        |> Random.andThen identity


think : Othello.Board -> Cmd Msg
think othello =
    if Othello.thinking othello then
        Task.perform (\_ -> OthelloResponded) (Process.sleep thinkingDelay)

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
        [ pageStyle model.theme
        , backgroundLayer model.theme
        , patternLayer model.theme model.pattern
        , eyeLayer model.theme model.screen model.time model.eyes
        , boidLayer model.theme model.screen model.boids
        , groundLayer model.theme model.screen model.walked (pace model / walkerSpeed)
        , Html.div
            [ Attr.style "position" "fixed"
            , Attr.style "inset" "0"
            , Attr.style "display" "flex"
            , Attr.style "justify-content" "center"
            , Attr.style "align-items" "center"
            , Attr.style "pointer-events" "none"
            ]
            [ boardLayer model ]
        , Cord.view (ink model.theme) (paper model.theme) lineWidth CordPulled (Motion.pullOffset model.pull)
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


pageStyle : Theme -> Html msg
pageStyle theme =
    Html.node "style"
        []
        [ Html.text
            (String.concat
                [ "html,body{margin:0;overflow:hidden;overscroll-behavior:none"
                , ";user-select:none;-webkit-user-select:none"
                , ";cursor:"
                , Cursor.css (ink theme) (paper theme) lineWidth False
                , "}"
                ]
            )
        ]


backgroundLayer : Theme -> Html msg
backgroundLayer theme =
    Html.div
        [ Attr.style "position" "fixed"
        , Attr.style "inset" "0"
        , Attr.style "background-color" (paper theme)
        , Attr.style "pointer-events" "none"
        ]
        []


eyeLayer : Theme -> Screen -> Float -> List Eye -> Html msg
eyeLayer theme screen now eyes =
    Svg.svg
        [ SvgAttr.width (String.fromFloat screen.width)
        , SvgAttr.height (String.fromFloat screen.height)
        , Attr.style "position" "fixed"
        , Attr.style "inset" "0"
        , Attr.style "pointer-events" "none"
        ]
        (List.filterMap (Eye.view (ink theme) (paper theme) lineWidth now) eyes)


boidLayer : Theme -> Screen -> List Boid -> Html msg
boidLayer theme screen boids =
    Svg.svg
        [ SvgAttr.width (String.fromFloat screen.width)
        , SvgAttr.height (String.fromFloat screen.height)
        , Attr.style "position" "fixed"
        , Attr.style "inset" "0"
        , Attr.style "pointer-events" "none"
        ]
        (List.concatMap (boidView theme screen) boids)


boidView : Theme -> Screen -> Boid -> List (Svg msg)
boidView theme screen boid =
    let
        heading =
            atan2 boid.vy boid.vx * 180 / pi
    in
    List.map
        (\( x, y ) -> Bird.view (ink theme) (paper theme) lineWidth { x = x, y = y, heading = heading })
        (Boid.wrapCopies screen boid)


groundLayer : Theme -> Screen -> Float -> Float -> Html msg
groundLayer theme screen walked rate =
    let
        level =
            ground screen

        here =
            strolled screen walked

        hurry =
            max 0 (rate - 1)

        swing =
            sin (walked / walkerStep * pi) * walkerSwing * min walkerFlail (1 + hurry * 0.4)

        tilt =
            negate (min walkerTilt (hurry * 3))
    in
    Svg.svg
        [ SvgAttr.width (String.fromFloat screen.width)
        , SvgAttr.height (String.fromFloat screen.height)
        , Attr.style "position" "fixed"
        , Attr.style "inset" "0"
        , Attr.style "pointer-events" "none"
        ]
        (Svg.rect
            [ SvgAttr.x "0"
            , SvgAttr.y (String.fromFloat level)
            , SvgAttr.width (String.fromFloat screen.width)
            , SvgAttr.height (String.fromFloat groundDepth)
            , SvgAttr.fill (paper theme)
            ]
            []
            :: Svg.line
                [ SvgAttr.x1 "0"
                , SvgAttr.y1 (String.fromFloat level)
                , SvgAttr.x2 (String.fromFloat screen.width)
                , SvgAttr.y2 (String.fromFloat level)
                , SvgAttr.stroke (ink theme)
                , SvgAttr.strokeWidth (String.fromFloat lineWidth)
                ]
                []
            :: List.map
                (\x ->
                    Walker.view (ink theme)
                        (paper theme)
                        lineWidth
                        { x = x
                        , ground = level - walkerLift
                        , swing = swing
                        , tilt = tilt
                        , standing = rate <= 0
                        }
                )
                (here :: seam screen.width here)
        )


strolled : Screen -> Float -> Float
strolled screen walked =
    wrap screen.width (screen.width - walked)


seam : Float -> Float -> List Float
seam span here =
    if here < Walker.width then
        [ here + span ]

    else if here > span - Walker.width then
        [ here - span ]

    else
        []


patternLayer : Theme -> String -> Html msg
patternLayer theme text =
    Html.pre
        [ Attr.style "position" "fixed"
        , Attr.style "inset" "0"
        , Attr.style "margin" "0"
        , Attr.style "overflow" "hidden"
        , Attr.style "font-family" "monospace"
        , Attr.style "font-size" (String.fromFloat patternFontSize ++ "px")
        , Attr.style "line-height" (String.fromFloat patternLineHeight)
        , Attr.style "color" (patternInk theme)
        , Attr.style "pointer-events" "none"
        , Attr.style "user-select" "none"
        ]
        [ Html.text text ]


board : Model -> Svg Msg
board model =
    Svg.svg
        [ SvgAttr.width (String.fromFloat boardSize)
        , SvgAttr.height (String.fromFloat boardSize)
        , SvgAttr.viewBox ("0 0 " ++ String.fromFloat boardSize ++ " " ++ String.fromFloat boardSize)
        , SvgAttr.style "overflow: visible"
        ]
        (List.concatMap (cellView model (boardPointer model)) Grid.cells)


cellView : Model -> Maybe Position -> Cell -> List (Svg Msg)
cellView model pointer cell =
    let
        ( column, row ) =
            cell

        ( shoveX, shoveY ) =
            shove pointer cell

        ( driftX, driftY ) =
            Motion.offset Grid.count model.shock model.time cell

        ( dx, dy ) =
            ( driftX + shoveX, driftY + shoveY )

        seen =
            content model cell

        opened =
            case seen of
                Face face ->
                    face /= Minesweeper.Hidden

                _ ->
                    False

        x =
            position column + gap / 2 + dx

        y =
            position row + gap / 2 + dy

        size =
            swell pointer cell
    in
    [ Svg.g
        [ SvgAttr.transform (about (x + cellSize / 2) (y + cellSize / 2) size)
        , SvgAttr.strokeWidth (String.fromFloat (lineWidth / size))
        ]
        (Svg.rect
            [ SvgAttr.x (String.fromFloat x)
            , SvgAttr.y (String.fromFloat y)
            , SvgAttr.width (String.fromFloat cellSize)
            , SvgAttr.height (String.fromFloat cellSize)
            , SvgAttr.fill
                (if opened then
                    ink model.theme

                 else
                    paper model.theme
                )
            , SvgAttr.stroke (ink model.theme)
            , SvgAttr.rx (String.fromFloat cellRadius)
            , SvgAttr.cursor (Cursor.css (ink model.theme) (paper model.theme) lineWidth True)
            , Svg.Events.onClick (CellClicked cell)
            ]
            []
            :: cellMarks model.theme seen x y
        )
    ]


content : Model -> Cell -> Content
content model cell =
    case model.play of
        Fresh ->
            Bare

        Mines game ->
            Face (Minesweeper.faceOf cell game)

        Discs othello ->
            case Othello.discAt cell othello of
                Nothing ->
                    Bare

                Just side ->
                    Stone side


cellMarks : Theme -> Content -> Float -> Float -> List (Svg msg)
cellMarks theme seen x y =
    case seen of
        Bare ->
            []

        Face face ->
            marks theme face x y

        Stone side ->
            [ disc theme side x y ]


disc : Theme -> Othello.Disc -> Float -> Float -> Svg msg
disc theme side x y =
    Svg.circle
        [ SvgAttr.cx (String.fromFloat (x + cellSize / 2))
        , SvgAttr.cy (String.fromFloat (y + cellSize / 2))
        , SvgAttr.r (String.fromFloat discRadius)
        , SvgAttr.fill
            (case side of
                Othello.Black ->
                    ink theme

                Othello.White ->
                    paper theme
            )
        , SvgAttr.stroke (ink theme)
        ]
        []


marks : Theme -> Minesweeper.Face -> Float -> Float -> List (Svg msg)
marks theme face x y =
    case face of
        Minesweeper.Hidden ->
            []

        Minesweeper.Blank ->
            []

        Minesweeper.Count count ->
            List.map (pip theme x y) (pipCells count)

        Minesweeper.Mine ->
            Skull.view (ink theme) (paper theme) cellSize x y


pip : Theme -> Float -> Float -> ( Int, Int ) -> Svg msg
pip theme x y ( column, row ) =
    Svg.circle
        [ SvgAttr.cx (String.fromFloat (x + pipOffset column))
        , SvgAttr.cy (String.fromFloat (y + pipOffset row))
        , SvgAttr.r (String.fromFloat pipRadius)
        , SvgAttr.fill (paper theme)
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


cellRadius : Float
cellRadius =
    3


margin : Float
margin =
    8


boardSize : Float
boardSize =
    spacing * toFloat Grid.count + margin * 2


position : Int -> Float
position index =
    margin + spacing * toFloat index


field : Screen -> Field
field screen =
    Field.around screen
        [ { left = screen.width / 2 - boardSize / 2
          , top = screen.height / 2 - boardSize / 2
          , right = screen.width / 2 + boardSize / 2
          , bottom = screen.height / 2 + boardSize / 2
          }
        ]



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



-- HOVER


hoverReach : Float
hoverReach =
    120


hoverGrow : Float
hoverGrow =
    0.35


hoverPush : Float
hoverPush =
    7


boardPointer : Model -> Maybe Position
boardPointer model =
    Maybe.map
        (\( px, py ) ->
            ( px - (model.screen.width / 2 - boardSize / 2)
            , py - (model.screen.height / 2 - boardSize / 2)
            )
        )
        model.pointer


reachOf : Maybe Position -> Cell -> Maybe ( Float, Float, Float )
reachOf pointer ( column, row ) =
    Maybe.map
        (\( px, py ) ->
            let
                dx =
                    position column + spacing / 2 - px

                dy =
                    position row + spacing / 2 - py
            in
            ( dx, dy, sqrt (dx * dx + dy * dy) )
        )
        pointer


swell : Maybe Position -> Cell -> Float
swell pointer cell =
    case reachOf pointer cell of
        Nothing ->
            1

        Just ( _, _, apart ) ->
            1 + hoverGrow * max 0 (1 - apart / spacing)


shove : Maybe Position -> Cell -> Vector
shove pointer cell =
    case reachOf pointer cell of
        Nothing ->
            ( 0, 0 )

        Just ( dx, dy, apart ) ->
            if apart == 0 || apart >= hoverReach then
                ( 0, 0 )

            else
                let
                    push =
                        hoverPush * sin (pi * apart / hoverReach)
                in
                ( dx / apart * push, dy / apart * push )


about : Float -> Float -> Float -> String
about midX midY size =
    String.concat
        [ "translate("
        , String.fromFloat midX
        , ","
        , String.fromFloat midY
        , ") scale("
        , String.fromFloat size
        , ") translate("
        , String.fromFloat (negate midX)
        , ","
        , String.fromFloat (negate midY)
        , ")"
        ]



-- GROUND


groundDepth : Float
groundDepth =
    52


ground : Screen -> Float
ground screen =
    screen.height - groundDepth



-- WALKER


walkerSpeed : Float
walkerSpeed =
    26


walkerStep : Float
walkerStep =
    26


walkerSwing : Float
walkerSwing =
    10


walkerSense : Float
walkerSense =
    220


walkerRush : Float
walkerRush =
    4


walkerFlail : Float
walkerFlail =
    2.8


walkerTilt : Float
walkerTilt =
    16


walkerLift : Float
walkerLift =
    3


walkerPanic : Float
walkerPanic =
    3


walkerPanicRange : Float
walkerPanicRange =
    60


walkerPanicFloor : Float
walkerPanicFloor =
    12


walkerLimit : Float
walkerLimit =
    30


walkerFocus : Float
walkerFocus =
    40


walkerBrake : Float
walkerBrake =
    1.8



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



-- THEME


type Theme
    = Light
    | Dark


toggle : Theme -> Theme
toggle theme =
    case theme of
        Light ->
            Dark

        Dark ->
            Light


ink : Theme -> String
ink theme =
    case theme of
        Light ->
            "#ababab"

        Dark ->
            "#545454"


paper : Theme -> String
paper theme =
    case theme of
        Light ->
            "#ffffff"

        Dark ->
            "#000000"


patternInk : Theme -> String
patternInk theme =
    case theme of
        Light ->
            "#eeeeee"

        Dark ->
            "#111111"
