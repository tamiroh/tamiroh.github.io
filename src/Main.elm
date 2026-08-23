module Main exposing (main)

import Bird
import Board
import Boid exposing (Boid)
import Browser
import Browser.Dom
import Browser.Events
import Cord
import Cursor
import Eye exposing (Eye)
import Field exposing (Field)
import Geometry exposing (Position, Screen)
import Grid exposing (Cell)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Json.Decode
import Millis exposing (Millis)
import Minesweeper
import Motion exposing (Pull, Shock)
import Othello
import Process
import Random
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Task
import Time
import Walker exposing (Walker)
import Wallpaper exposing (Wallpaper)



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
    , wallpaper : Wallpaper
    , shock : Maybe Shock
    , elapsed : Millis
    , theme : Theme
    , cord : Maybe Pull
    , screen : Screen
    , boids : List Boid
    , pointer : Maybe Position
    , walker : Walker
    , eyes : List Eye
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
      , walker = Walker.start
      , eyes = []
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
    | TouchEnded
    | PointerLeft
    | GotViewport Browser.Dom.Viewport
    | WindowResized Int Int
    | WallpaperMade Wallpaper
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
                , shock = Motion.advance (Motion.shockLifetime Grid.count) delta model.shock
                , cord = Motion.advance Motion.pullDuration delta model.cord
                , boids = Boid.flock delta (field model.screen) model.pointer model.boids
                , walker = Walker.step delta model.screen (ground model.screen) model.pointer model.walker
                , eyes = List.filterMap (Eye.alive model.elapsed model.pointer) model.eyes
              }
            , Cmd.none
            )

        SecondPassed ->
            ( model
            , Cmd.batch
                [ Random.generate WallpaperMade (Wallpaper.generator model.screen)
                , Random.generate EyeOpened (Eye.generator (field model.screen) model.elapsed)
                ]
            )

        -- ENVIRONMENT
        PointerMoved point ->
            ( { model | pointer = Just point }, Cmd.none )

        TouchEnded ->
            ( model, Task.perform (\_ -> PointerLeft) (Process.sleep pointerFade) )

        PointerLeft ->
            ( { model | pointer = Nothing }, Cmd.none )

        GotViewport viewport ->
            let
                screen =
                    { width = viewport.viewport.width, height = viewport.viewport.height }
            in
            ( { model | screen = screen }
            , Cmd.batch
                [ Random.generate BoidsPlaced (Boid.generator (field screen))
                , Random.generate WallpaperMade (Wallpaper.generator screen)
                ]
            )

        WindowResized width height ->
            ( { model | screen = { width = toFloat width, height = toFloat height } }, Cmd.none )

        -- RANDOM
        WallpaperMade made ->
            ( { model | wallpaper = made }, Cmd.none )

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
        , Wallpaper.view (wallpaperInk model.theme) model.wallpaper
        , eyeLayer model.theme model.screen model.elapsed model.eyes
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
        , Cord.view (ink model.theme) (paper model.theme) lineWidth CordPulled (Motion.pullOffset model.cord)
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
        [ Board.view
            { ink = ink model.theme
            , paper = paper model.theme
            , stroke = lineWidth
            , screen = model.screen
            , pointer = model.pointer
            , drift = Motion.offset Grid.count model.shock model.elapsed
            , content = contentAt model
            , click = CellClicked
            }
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
            :: Walker.view (ink theme) (paper theme) lineWidth screen level walker
        )



-- LAYOUT


lineWidth : Float
lineWidth =
    2


field : Screen -> Field
field screen =
    Field.around screen
        [ { left = screen.width / 2 - Board.size / 2
          , top = screen.height / 2 - Board.size / 2
          , right = screen.width / 2 + Board.size / 2
          , bottom = screen.height / 2 + Board.size / 2
          }
        ]



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


wallpaperInk : Theme -> String
wallpaperInk theme =
    case theme of
        Light ->
            "#eeeeee"

        Dark ->
            "#111111"
