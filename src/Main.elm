module Main exposing (main)

import Bird
import Board
import Boid exposing (Boid)
import Browser
import Browser.Dom
import Browser.Events
import Cord exposing (Pull)
import Cursor
import Eye exposing (Eye)
import Field exposing (Field, Obstacle)
import Geometry exposing (Position)
import Grid exposing (Cell)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Json.Decode
import Millis exposing (Millis)
import Minesweeper
import Othello
import Process
import Random
import Screen exposing (Screen)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Task
import Time
import Torus exposing (Torus)
import Walker exposing (Walker)
import Wallpaper exposing (Pattern, Rendered)



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
    , wallpaper : Rendered
    , shock : Maybe Board.Shock
    , elapsed : Millis
    , theme : Theme
    , cord : Maybe Pull
    , screen : Screen
    , boids : List Boid
    , pointer : Maybe Position
    , walker : Walker
    , eyes : List Eye
    , blocks : List Obstacle
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { play = Fresh
      , wallpaper = Wallpaper.blank
      , shock = Nothing
      , elapsed = 0
      , theme = Light
      , cord = Nothing
      , screen = { width = 0, height = 0 }
      , boids = []
      , pointer = Nothing
      , walker = Walker.new
      , eyes = []
      , blocks = []
      }
    , Task.perform GotViewport Browser.Dom.getViewport
    )


type Play
    = Fresh
    | Mines Minesweeper.Game
    | Discs Othello.Board


over : Play -> Bool
over play =
    case play of
        Fresh ->
            False

        Mines game ->
            Minesweeper.finished game

        Discs othello ->
            Othello.isOver othello



-- UPDATE


type Msg
    = CellClicked Cell
    | CordPulled
    | GameStarted Play
    | OthelloResponded
    | AnimationFramePassed Float
    | SecondPassed
    | PointerMoved Position
    | FieldClicked Position
    | TouchEnded
    | PointerLeft
    | BlockPlaced Obstacle
    | GotViewport Browser.Dom.Viewport
    | WindowResized Int Int
    | PatternChosen Pattern
    | BoidsPlaced (List Boid)
    | EyeOpened (Maybe Eye)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        boardBlock : Screen -> Obstacle
        boardBlock screen =
            Field.square ( screen.width / 2, screen.height / 2 ) Board.size

        field : Screen -> Field
        field screen =
            Field.around screen (boardBlock screen :: model.blocks)

        thinkingDelay : Millis
        thinkingDelay =
            420

        think : Othello.Board -> Cmd Msg
        think othello =
            if Othello.thinking othello then
                Task.perform (\_ -> OthelloResponded) (Process.sleep thinkingDelay)

            else
                Cmd.none
    in
    case msg of
        -- PLAYER
        CellClicked cell ->
            let
                startShock : Model -> Model
                startShock target =
                    { target | shock = Just { origin = cell, elapsed = 0 } }

                startGenerator : Random.Generator Play
                startGenerator =
                    Random.uniform
                        (Random.map (\game -> Mines (Minesweeper.reveal cell game)) (Minesweeper.start cell))
                        [ Random.constant (Discs Othello.new) ]
                        |> Random.andThen identity
            in
            case model.play of
                Fresh ->
                    ( startShock model, Random.generate GameStarted startGenerator )

                Mines game ->
                    if Minesweeper.finished game then
                        ( model, Cmd.none )

                    else
                        ( startShock { model | play = Mines (Minesweeper.reveal cell game) }, Cmd.none )

                Discs othello ->
                    case Othello.play cell othello of
                        Nothing ->
                            ( model, Cmd.none )

                        Just next ->
                            ( startShock { model | play = Discs next }, think next )

        CordPulled ->
            ( { model | theme = toggle model.theme, cord = Just { elapsed = 0 } }, Cmd.none )

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
                | elapsed = model.elapsed + delta
                , shock = Board.step delta model.shock
                , cord = Cord.step delta model.cord
                , boids = Boid.step delta (field model.screen) model.pointer model.boids
                , walker = Walker.step delta (Torus.around model.screen) (groundLevel model.screen) model.pointer model.walker
                , eyes = List.filterMap (Eye.step model.elapsed model.pointer) model.eyes
              }
            , Cmd.none
            )

        SecondPassed ->
            ( model
            , Cmd.batch
                [ Random.generate PatternChosen Wallpaper.generator
                , Random.generate EyeOpened (Eye.generator (field model.screen) model.elapsed)
                ]
            )

        -- ENVIRONMENT
        PointerMoved point ->
            ( { model | pointer = Just point }, Cmd.none )

        FieldClicked point ->
            let
                blockSpan : Float
                blockSpan =
                    54

                blockGenerator : Random.Generator Obstacle
                blockGenerator =
                    Random.uniform (Field.triangle point blockSpan)
                        [ Field.heart point blockSpan
                        , Field.square point blockSpan
                        ]
            in
            ( model, Random.generate BlockPlaced blockGenerator )

        TouchEnded ->
            let
                pointerFade : Millis
                pointerFade =
                    350
            in
            ( model, Task.perform (\_ -> PointerLeft) (Process.sleep pointerFade) )

        PointerLeft ->
            ( { model | pointer = Nothing }, Cmd.none )

        BlockPlaced block ->
            let
                blockLimit : Int
                blockLimit =
                    12
            in
            ( { model | blocks = List.take blockLimit (block :: model.blocks) }, Cmd.none )

        GotViewport viewport ->
            let
                screen =
                    { width = viewport.viewport.width, height = viewport.viewport.height }
            in
            ( { model | screen = screen }
            , Cmd.batch
                [ Random.generate BoidsPlaced (Boid.generator (field screen))
                , Random.generate PatternChosen Wallpaper.generator
                ]
            )

        WindowResized width height ->
            ( { model | screen = { width = toFloat width, height = toFloat height } }, Cmd.none )

        -- RANDOM
        PatternChosen pattern ->
            ( { model | wallpaper = Wallpaper.render model.screen pattern }, Cmd.none )

        BoidsPlaced boids ->
            ( { model | boids = boids }, Cmd.none )

        EyeOpened opened ->
            case opened of
                Nothing ->
                    ( model, Cmd.none )

                Just eye ->
                    ( { model | eyes = eye :: model.eyes }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    let
        pointerDecoder : Json.Decode.Decoder Msg
        pointerDecoder =
            Json.Decode.map PointerMoved positionDecoder
    in
    Sub.batch
        [ Time.every 1000 (\_ -> SecondPassed)
        , Browser.Events.onAnimationFrameDelta AnimationFramePassed
        , Browser.Events.onResize WindowResized
        , Browser.Events.onMouseMove pointerDecoder
        ]



-- VIEW


view : Model -> Html Msg
view model =
    let
        wallpaperLook : Wallpaper.Look
        wallpaperLook =
            case model.theme of
                Light ->
                    { ink = "#eeeeee" }

                Dark ->
                    { ink = "#111111" }

        pageStyle : Html msg
        pageStyle =
            Html.node "style"
                []
                [ Html.text
                    (String.concat
                        [ "html,body{margin:0;overflow:hidden;overscroll-behavior:none"
                        , ";user-select:none;-webkit-user-select:none"
                        , ";cursor:"
                        , Cursor.css (look model.theme) Cursor.Empty
                        , "}"
                        ]
                    )
                ]
    in
    Html.div
        [ Html.Events.on "touchend" (Json.Decode.succeed TouchEnded)
        , Html.Events.on "touchcancel" (Json.Decode.succeed TouchEnded)
        ]
        [ pageStyle
        , backgroundLayer model.theme
        , Wallpaper.view wallpaperLook model.wallpaper
        , eyeLayer model.theme model.screen model.elapsed model.eyes
        , blockLayer model.theme model.screen model.blocks
        , boidLayer model.theme model.screen model.boids
        , groundLayer model.theme model.screen model.walker
        , Html.div
            [ Attr.style "position" "fixed"
            , Attr.style "inset" "0"
            , Attr.style "display" "flex"
            , Attr.style "justify-content" "center"
            , Attr.style "align-items" "center"
            , Attr.style "pointer-events" "none"
            ]
            [ boardLayer model ]
        , Cord.view (look model.theme) CordPulled model.cord
        ]


backgroundLayer : Theme -> Html Msg
backgroundLayer theme =
    Html.div
        [ Attr.style "position" "fixed"
        , Attr.style "inset" "0"
        , Attr.style "background-color" (paper theme)
        , Html.Events.on "click" (Json.Decode.map FieldClicked positionDecoder)
        ]
        []


eyeLayer : Theme -> Screen -> Millis -> List Eye -> Html msg
eyeLayer theme screen now eyes =
    Svg.svg
        [ SvgAttr.width (String.fromFloat screen.width)
        , SvgAttr.height (String.fromFloat screen.height)
        , Attr.style "position" "fixed"
        , Attr.style "inset" "0"
        , Attr.style "pointer-events" "none"
        ]
        (List.filterMap (Eye.view (look theme) now) eyes)


blockLayer : Theme -> Screen -> List Obstacle -> Html msg
blockLayer theme screen blocks =
    let
        corner : Position -> String
        corner ( x, y ) =
            String.fromFloat x ++ "," ++ String.fromFloat y

        blockView : Obstacle -> Svg msg
        blockView block =
            Svg.polygon
                [ SvgAttr.points (String.join " " (List.map corner (Field.outline block)))
                , SvgAttr.fill (paper theme)
                , SvgAttr.stroke (ink theme)
                , SvgAttr.strokeWidth (String.fromFloat lineWidth)
                , SvgAttr.strokeLinejoin "round"
                ]
                []
    in
    Svg.svg
        [ SvgAttr.width (String.fromFloat screen.width)
        , SvgAttr.height (String.fromFloat screen.height)
        , Attr.style "position" "fixed"
        , Attr.style "inset" "0"
        , Attr.style "pointer-events" "none"
        ]
        (List.map blockView blocks)


boidLayer : Theme -> Screen -> List Boid -> Html msg
boidLayer theme screen boids =
    let
        torus =
            Torus.around screen

        boidView : Boid -> List (Svg msg)
        boidView boid =
            let
                heading =
                    atan2 boid.vy boid.vx * 180 / pi
            in
            List.map
                (\( x, y ) -> Bird.view (look theme) { x = x, y = y, heading = heading })
                (Boid.places torus boid)
    in
    Svg.svg
        [ SvgAttr.width (String.fromFloat screen.width)
        , SvgAttr.height (String.fromFloat screen.height)
        , Attr.style "position" "fixed"
        , Attr.style "inset" "0"
        , Attr.style "pointer-events" "none"
        ]
        (List.concatMap boidView boids)


groundLayer : Theme -> Screen -> Walker -> Html msg
groundLayer theme screen walker =
    let
        level =
            groundLevel screen
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
            :: Walker.view (look theme) (Torus.around screen) level walker
        )


boardLayer : Model -> Html Msg
boardLayer model =
    let
        contentAt : Cell -> Board.Content
        contentAt cell =
            case model.play of
                Fresh ->
                    Board.Bare

                Mines game ->
                    case Minesweeper.faceOf cell game of
                        Minesweeper.Hidden ->
                            Board.Bare

                        Minesweeper.Blank ->
                            Board.Open Board.Blank

                        Minesweeper.Count count ->
                            Board.Open (Board.Pips count)

                        Minesweeper.Mine ->
                            Board.Open Board.Mine

                Discs othello ->
                    case Othello.discAt cell othello of
                        Nothing ->
                            Board.Bare

                        Just Othello.Black ->
                            Board.Piece Board.Dark

                        Just Othello.White ->
                            Board.Piece Board.Light
    in
    Html.div
        [ Attr.style "pointer-events"
            (if over model.play then
                "none"

             else
                "auto"
            )
        ]
        [ Svg.map CellClicked
            (Board.view
                { look = look model.theme
                , screen = model.screen
                , pointer = model.pointer
                , shock = model.shock
                , elapsed = model.elapsed
                , content = contentAt
                }
            )
        ]



-- DECODE


positionDecoder : Json.Decode.Decoder Position
positionDecoder =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.field "clientX" Json.Decode.float)
        (Json.Decode.field "clientY" Json.Decode.float)



-- GROUND


groundDepth : Float
groundDepth =
    52


groundLevel : Screen -> Float
groundLevel screen =
    screen.height - groundDepth



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


lineWidth : Float
lineWidth =
    2


look : Theme -> { ink : String, paper : String, stroke : Float }
look theme =
    { ink = ink theme, paper = paper theme, stroke = lineWidth }


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
