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
import Transform
import Walker exposing (Walker)
import Wallpaper exposing (Pattern, Rendered)
import Wobble



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
    , walkers : List Walker
    , eyes : List Eye
    , blocks : List Field.Body
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
      , walkers = []
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
    | BlockPlaced Field.Body
    | GotViewport Browser.Dom.Viewport
    | WindowResized Int Int
    | PatternChosen Pattern
    | BoidsPlaced (List Boid)
    | BlocksPlaced (List (Maybe Field.Body))
    | WalkersPlaced (List Walker)
    | WalkersSpoke (List Walker)
    | EyeOpened (Maybe Eye)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        boardBlock : Screen -> Obstacle
        boardBlock screen =
            Field.square ( screen.width / 2, screen.height / 2 ) Board.size

        field : Screen -> Field
        field screen =
            Field.around screen (boardBlock screen :: List.map .shape model.blocks)

        thinkingDelay : Millis
        thinkingDelay =
            420

        think : Othello.Board -> Cmd Msg
        think othello =
            if Othello.thinking othello then
                Task.perform (\_ -> OthelloResponded) (Process.sleep thinkingDelay)

            else
                Cmd.none

        blockSpan : Float
        blockSpan =
            54

        driftSpeed : Float
        driftSpeed =
            30

        blockGenerator : Position -> Random.Generator Field.Body
        blockGenerator point =
            Random.map2
                (\shape heading ->
                    { shape = shape
                    , velocity = ( cos heading * driftSpeed, sin heading * driftSpeed )
                    }
                )
                (Random.uniform (Field.triangle point blockSpan)
                    [ Field.heart point blockSpan
                    , Field.square point blockSpan
                    ]
                )
                (Random.float 0 (2 * pi))
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
                , walkers = List.map (Walker.step delta model.elapsed (Torus.around model.screen) (groundLevel model.screen) model.pointer) model.walkers
                , eyes = List.filterMap (Eye.step model.elapsed model.pointer) model.eyes
                , blocks =
                    model.blocks
                        |> List.map (Field.drift delta)
                        |> List.map (Field.bounce model.screen (boardBlock model.screen))
                        |> List.map (Field.wrap (Torus.around model.screen))
                        |> Field.collide
                        |> List.map (\body -> { body | shape = Field.spin delta body.shape })
              }
            , Cmd.none
            )

        SecondPassed ->
            ( model
            , Cmd.batch
                (Random.generate PatternChosen Wallpaper.generator
                    :: Random.generate WalkersSpoke (Walker.speakAll model.elapsed model.walkers)
                    :: (if model.theme == Dark then
                            [ Random.generate EyeOpened (Eye.generator (field model.screen) model.elapsed) ]

                        else
                            []
                       )
                )
            )

        -- ENVIRONMENT
        PointerMoved point ->
            ( { model | pointer = Just point }, Cmd.none )

        FieldClicked point ->
            ( model, Random.generate BlockPlaced (blockGenerator point) )

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

                walkerCount : Int
                walkerCount =
                    3

                blockCount : Int
                blockCount =
                    4

                blockSpotAttempts : Int
                blockSpotAttempts =
                    8

                blockAt : Random.Generator (Maybe Field.Body)
                blockAt =
                    Field.spot (blockSpan / 2) (field screen) blockSpotAttempts
                        |> Random.andThen
                            (\maybePoint ->
                                case maybePoint of
                                    Just point ->
                                        Random.map Just (blockGenerator point)

                                    Nothing ->
                                        Random.constant Nothing
                            )
            in
            ( { model | screen = screen }
            , Cmd.batch
                [ Random.generate BoidsPlaced (Boid.generator (field screen))
                , Random.generate WalkersPlaced (Random.list walkerCount (Walker.generator screen))
                , Random.generate BlocksPlaced (Random.list blockCount blockAt)
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

        BlocksPlaced blocks ->
            ( { model | blocks = List.filterMap identity blocks }, Cmd.none )

        WalkersPlaced walkers ->
            ( { model | walkers = walkers }, Cmd.none )

        WalkersSpoke walkers ->
            ( { model | walkers = walkers }, Cmd.none )

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
                    { ink = "#242424" }

        cursorBulge : Float
        cursorBulge =
            1.2

        pageStyle : Html msg
        pageStyle =
            Html.node "style"
                []
                [ Html.text
                    (String.concat
                        [ "html,body{margin:0;overflow:hidden;overscroll-behavior:none"
                        , ";user-select:none;-webkit-user-select:none"
                        , ";cursor:"
                        , Cursor.css 1 (look model.theme) Cursor.Empty
                        , "}"
                        , "html:active,body:active{cursor:"
                        , Cursor.css cursorBulge (look model.theme) Cursor.Empty
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
        , blockLayer model.theme model.screen model.elapsed model.blocks
        , boidLayer model.theme model.screen model.boids
        , groundLayer model.theme model.screen model.elapsed model.walkers
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


blockLayer : Theme -> Screen -> Millis -> List Field.Body -> Html msg
blockLayer theme screen now blocks =
    let
        blockRadius : Float
        blockRadius =
            6

        point : Position -> String
        point ( x, y ) =
            String.fromFloat x ++ "," ++ String.fromFloat y

        apart : Position -> Position -> Float
        apart ( ax, ay ) ( bx, by ) =
            sqrt ((ax - bx) ^ 2 + (ay - by) ^ 2)

        approach : Position -> Float -> Position -> Position
        approach ( cx, cy ) distance ( tx, ty ) =
            let
                length =
                    apart ( cx, cy ) ( tx, ty )
            in
            if length == 0 then
                ( cx, cy )

            else
                ( cx + (tx - cx) / length * distance, cy + (ty - cy) / length * distance )

        fillet : Position -> Position -> Position -> ( Position, String )
        fillet prev corner next =
            let
                near =
                    min blockRadius (min (apart corner prev) (apart corner next) / 2)

                entry =
                    approach corner near prev

                exit =
                    approach corner near next
            in
            ( entry, "L " ++ point entry ++ " Q " ++ point corner ++ " " ++ point exit )

        rotateLeft : List a -> List a
        rotateLeft list =
            case list of
                [] ->
                    []

                first :: rest ->
                    rest ++ [ first ]

        rotateRight : List a -> List a
        rotateRight list =
            case List.reverse list of
                [] ->
                    []

                last :: rest ->
                    last :: List.reverse rest

        rounded : List Position -> String
        rounded corners =
            case List.map3 fillet (rotateRight corners) corners (rotateLeft corners) of
                [] ->
                    ""

                ( entry, segment ) :: rest ->
                    "M " ++ point entry ++ " " ++ segment ++ " " ++ String.join " " (List.map Tuple.second rest) ++ " Z"

        wobbleSeed : Obstacle -> Float
        wobbleSeed shape =
            let
                ( mx, my ) =
                    Field.middle shape
            in
            mx * 0.1 + my * 0.17

        blockView : Field.Body -> Svg msg
        blockView block =
            Svg.path
                [ SvgAttr.d (rounded (Field.outline block.shape))
                , SvgAttr.fill (paper theme)
                , SvgAttr.stroke (ink theme)
                , SvgAttr.strokeWidth (String.fromFloat lineWidth)
                , SvgAttr.strokeLinejoin "round"
                , SvgAttr.transform (Transform.translate (Wobble.at now (wobbleSeed block.shape)))
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


groundLayer : Theme -> Screen -> Millis -> List Walker -> Html msg
groundLayer theme screen now walkers =
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
            :: List.concatMap (Walker.view (look theme) now (Torus.around screen) level) walkers
        )


boardLayer : Model -> Html Msg
boardLayer model =
    let
        doorUrl : String
        doorUrl =
            "https://github.com/tamiroh/tamiroh.github.io"

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

                        Minesweeper.Door ->
                            Board.Open Board.Door

                Discs othello ->
                    case Othello.discAt cell othello of
                        Nothing ->
                            Board.Bare

                        Just Othello.Black ->
                            Board.Piece Board.Dark

                        Just Othello.White ->
                            Board.Piece Board.Light

        linkAt : Cell -> Maybe String
        linkAt cell =
            case model.play of
                Mines game ->
                    if Minesweeper.faceOf cell game == Minesweeper.Door then
                        Just doorUrl

                    else
                        Nothing

                _ ->
                    Nothing
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
                , link = linkAt
                }
            )
        ]



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



-- HELPERS


positionDecoder : Json.Decode.Decoder Position
positionDecoder =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.field "clientX" Json.Decode.float)
        (Json.Decode.field "clientY" Json.Decode.float)
