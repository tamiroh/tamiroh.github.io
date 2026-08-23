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
import Motion exposing (Shock)
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
    , shock : Maybe Shock
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


blockLimit : Int
blockLimit =
    12


blockSpan : Float
blockSpan =
    54


blockGenerator : Position -> Random.Generator Obstacle
blockGenerator point =
    Random.uniform (Field.triangle point blockSpan)
        [ Field.heart point blockSpan
        , Field.square point blockSpan
        ]


pointerFade : Millis
pointerFade =
    350


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
                    ( startShock cell model, Random.generate GameStarted (startGenerator cell) )

                Mines game ->
                    if Minesweeper.finished game then
                        ( model, Cmd.none )

                    else
                        ( startShock cell { model | play = Mines (Minesweeper.reveal cell game) }, Cmd.none )

                Discs othello ->
                    case Othello.play cell othello of
                        Nothing ->
                            ( model, Cmd.none )

                        Just next ->
                            ( startShock cell { model | play = Discs next }, think next )

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
                , shock = Motion.advance (Motion.shockLifetime Grid.side) delta model.shock
                , cord = Motion.advance Cord.pullDuration delta model.cord
                , boids = Boid.step delta (field model) model.pointer model.boids
                , walker = Walker.step delta (Torus.around model.screen) (ground model.screen) model.pointer model.walker
                , eyes = List.filterMap (Eye.alive model.elapsed model.pointer) model.eyes
              }
            , Cmd.none
            )

        SecondPassed ->
            ( model
            , Cmd.batch
                [ Random.generate PatternChosen Wallpaper.generator
                , Random.generate EyeOpened (Eye.generator (field model) model.elapsed)
                ]
            )

        -- ENVIRONMENT
        PointerMoved point ->
            ( { model | pointer = Just point }, Cmd.none )

        FieldClicked point ->
            ( model, Random.generate BlockPlaced (blockGenerator point) )

        TouchEnded ->
            ( model, Task.perform (\_ -> PointerLeft) (Process.sleep pointerFade) )

        PointerLeft ->
            ( { model | pointer = Nothing }, Cmd.none )

        BlockPlaced block ->
            ( { model | blocks = List.take blockLimit (block :: model.blocks) }, Cmd.none )

        GotViewport viewport ->
            let
                screen =
                    { width = viewport.viewport.width, height = viewport.viewport.height }
            in
            ( { model | screen = screen }
            , Cmd.batch
                [ Random.generate BoidsPlaced (Boid.generator (field { model | screen = screen }))
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


startGenerator : Cell -> Random.Generator Play
startGenerator cell =
    Random.uniform
        (Random.map (\game -> Mines (Minesweeper.reveal cell game)) (Minesweeper.start cell))
        [ Random.constant (Discs Othello.new) ]
        |> Random.andThen identity


startShock : Cell -> Model -> Model
startShock cell model =
    { model | shock = Just { origin = cell, elapsed = 0 } }



-- VIEW


view : Model -> Html Msg
view model =
    Html.div
        [ Html.Events.on "touchend" (Json.Decode.succeed TouchEnded)
        , Html.Events.on "touchcancel" (Json.Decode.succeed TouchEnded)
        ]
        [ pageStyle model.theme
        , backgroundLayer model.theme
        , Wallpaper.view (wallpaperLook model.theme) model.wallpaper
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


contentAt : Model -> Cell -> Board.Content
contentAt model cell =
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
        [ Svg.map CellClicked
            (Board.view
                { look = look model.theme
                , screen = model.screen
                , pointer = model.pointer
                , drift = Motion.offset Grid.side model.shock model.elapsed
                , content = contentAt model
                }
            )
        ]


pageStyle : Theme -> Html msg
pageStyle theme =
    Html.node "style"
        []
        [ Html.text
            (String.concat
                [ "html,body{margin:0;overflow:hidden;overscroll-behavior:none"
                , ";user-select:none;-webkit-user-select:none"
                , ";cursor:"
                , Cursor.css (look theme) Cursor.Empty
                , "}"
                ]
            )
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


blockLayer : Theme -> Screen -> List Obstacle -> Html msg
blockLayer theme screen blocks =
    Svg.svg
        [ SvgAttr.width (String.fromFloat screen.width)
        , SvgAttr.height (String.fromFloat screen.height)
        , Attr.style "position" "fixed"
        , Attr.style "inset" "0"
        , Attr.style "pointer-events" "none"
        ]
        (List.map (blockView theme) blocks)


blockView : Theme -> Obstacle -> Svg msg
blockView theme block =
    Svg.polygon
        [ SvgAttr.points (String.join " " (List.map corner (Field.outline block)))
        , SvgAttr.fill (paper theme)
        , SvgAttr.stroke (ink theme)
        , SvgAttr.strokeWidth (String.fromFloat lineWidth)
        , SvgAttr.strokeLinejoin "round"
        ]
        []


corner : Position -> String
corner ( x, y ) =
    String.fromFloat x ++ "," ++ String.fromFloat y


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


boidLayer : Theme -> Screen -> List Boid -> Html msg
boidLayer theme screen boids =
    Svg.svg
        [ SvgAttr.width (String.fromFloat screen.width)
        , SvgAttr.height (String.fromFloat screen.height)
        , Attr.style "position" "fixed"
        , Attr.style "inset" "0"
        , Attr.style "pointer-events" "none"
        ]
        (List.concatMap (boidView theme (Torus.around screen)) boids)


boidView : Theme -> Torus -> Boid -> List (Svg msg)
boidView theme torus boid =
    let
        heading =
            atan2 boid.vy boid.vx * 180 / pi
    in
    List.map
        (\( x, y ) -> Bird.view (look theme) { x = x, y = y, heading = heading })
        (Boid.places torus boid)


groundLayer : Theme -> Screen -> Walker -> Html msg
groundLayer theme screen walker =
    let
        level =
            ground screen
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



-- LAYOUT


lineWidth : Float
lineWidth =
    2


field : Model -> Field
field model =
    Field.around model.screen (boardBlock model.screen :: model.blocks)


boardBlock : Screen -> Obstacle
boardBlock screen =
    Field.square ( screen.width / 2, screen.height / 2 ) Board.size



-- OTHELLO


thinkingDelay : Millis
thinkingDelay =
    420


think : Othello.Board -> Cmd Msg
think othello =
    if Othello.thinking othello then
        Task.perform (\_ -> OthelloResponded) (Process.sleep thinkingDelay)

    else
        Cmd.none



-- GROUND


groundDepth : Float
groundDepth =
    52


ground : Screen -> Float
ground screen =
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


wallpaperLook : Theme -> Wallpaper.Look
wallpaperLook theme =
    case theme of
        Light ->
            { ink = "#eeeeee" }

        Dark ->
            { ink = "#111111" }
