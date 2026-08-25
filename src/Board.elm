module Board exposing (Content(..), Mark(..), Shade(..), Shock, size, step, view)

import Cursor
import Dice
import Door
import Geometry exposing (Position, Vector)
import Grid exposing (Cell)
import Html.Attributes as Attr
import Millis exposing (Millis)
import Piece
import Screen exposing (Screen)
import Skull
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Svg.Events
import Transform
import Wobble



-- BOARD


type alias Look =
    { ink : String
    , paper : String
    , stroke : Float
    }


type alias Scene =
    { look : Look
    , screen : Screen
    , pointer : Maybe Position
    , shock : Maybe Shock
    , elapsed : Millis
    , content : Cell -> Content
    , link : Cell -> Maybe String
    }


type alias Shock =
    { origin : Cell
    , elapsed : Millis
    }


type Content
    = Bare
    | Open Mark
    | Piece Shade


type Mark
    = Blank
    | Pips Int
    | Mine
    | Door


type Shade
    = Dark
    | Light


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


size : Float
size =
    spacing * toFloat Grid.side + margin * 2


position : Int -> Float
position index =
    margin + spacing * toFloat index


about : Position -> Float -> String
about ( midX, midY ) factor =
    String.join " "
        [ Transform.translate ( midX, midY )
        , Transform.scale factor
        , Transform.translate ( negate midX, negate midY )
        ]



-- VIEW


view : Scene -> Svg Cell
view scene =
    Svg.svg
        [ SvgAttr.width (String.fromFloat size)
        , SvgAttr.height (String.fromFloat size)
        , SvgAttr.viewBox ("0 0 " ++ String.fromFloat size ++ " " ++ String.fromFloat size)
        , SvgAttr.style "overflow: visible"
        ]
        (List.concatMap (cellView scene (pointerOnBoard scene.screen scene.pointer)) Grid.cells)


cellView : Scene -> Maybe Position -> Cell -> List (Svg Cell)
cellView scene pointer cell =
    let
        ( column, row ) =
            cell

        hovered =
            hover pointer cell

        ( shiftX, shiftY ) =
            hovered.shift

        ( driftX, driftY ) =
            driftAt scene cell

        seen =
            scene.content cell

        opened =
            case seen of
                Open _ ->
                    True

                _ ->
                    False

        x =
            position column + gap / 2 + driftX + shiftX

        y =
            position row + gap / 2 + driftY + shiftY

        swelled =
            hovered.scale

        href =
            scene.link cell

        rectAttrs =
            [ SvgAttr.x (String.fromFloat x)
            , SvgAttr.y (String.fromFloat y)
            , SvgAttr.width (String.fromFloat cellSize)
            , SvgAttr.height (String.fromFloat cellSize)
            , SvgAttr.fill
                (if opened then
                    scene.look.ink

                 else
                    scene.look.paper
                )
            , SvgAttr.stroke scene.look.ink
            , SvgAttr.rx (String.fromFloat cellRadius)
            , SvgAttr.cursor (Cursor.css 1 scene.look Cursor.Clickable)
            ]
                ++ (if href == Nothing then
                        [ Svg.Events.onClick cell ]

                    else
                        []
                   )

        body =
            Svg.rect rectAttrs [] :: cellMarks scene seen ( x, y )
    in
    [ Svg.g
        [ SvgAttr.transform (about ( x + cellSize / 2, y + cellSize / 2 ) swelled)
        , SvgAttr.strokeWidth (String.fromFloat (scene.look.stroke / swelled))
        ]
        (case href of
            Just url ->
                [ Svg.node "a"
                    [ Attr.attribute "href" url
                    , Attr.attribute "target" "_blank"
                    , Attr.attribute "rel" "noopener noreferrer"
                    ]
                    body
                ]

            Nothing ->
                body
        )
    ]


cellMarks : Scene -> Content -> Position -> List (Svg Cell)
cellMarks scene seen ( x, y ) =
    case seen of
        Bare ->
            []

        Open Blank ->
            []

        Open (Pips count) ->
            Dice.view cellSize ( x, y ) scene.look.paper count

        Open Mine ->
            Skull.view { ink = scene.look.ink, paper = scene.look.paper } cellSize ( x, y )

        Open Door ->
            Door.view { ink = scene.look.ink, paper = scene.look.paper } cellSize ( x, y )

        Piece shade ->
            [ disc scene shade ( x, y ) ]


disc : Scene -> Shade -> Position -> Svg msg
disc scene shade ( x, y ) =
    Piece.view discRadius ( x + cellSize / 2, y + cellSize / 2 ) scene.look.ink scene.look.paper (shade == Dark)



-- SHOCK


shockAmplitude : Float
shockAmplitude =
    140


shockDuration : Millis
shockDuration =
    700


shockDelay : Millis
shockDelay =
    65


shockLifetime : Millis
shockLifetime =
    shockDuration + shockDelay * maxDistance


step : Millis -> Maybe Shock -> Maybe Shock
step delta shock =
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


maxDistance : Float
maxDistance =
    sqrt 2 * (toFloat Grid.side - 1)


displacement : Maybe Shock -> Cell -> Vector
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



-- DRIFT


driftAt : Scene -> Cell -> Vector
driftAt scene ( column, row ) =
    let
        ( shockX, shockY ) =
            displacement scene.shock ( column, row )

        ( driftX, driftY ) =
            Wobble.at scene.elapsed (toFloat (column * 3 + row * 5))
    in
    ( shockX + driftX, shockY + driftY )



-- MARKS


discRadius : Float
discRadius =
    cellSize * 0.34



-- HOVER


pointerOnBoard : Screen -> Maybe Position -> Maybe Position
pointerOnBoard screen pointer =
    Maybe.map
        (\( px, py ) ->
            ( px - (screen.width / 2 - size / 2)
            , py - (screen.height / 2 - size / 2)
            )
        )
        pointer


hoverReach : Float
hoverReach =
    120


hoverSwell : Float
hoverSwell =
    0.35


hoverShift : Float
hoverShift =
    7


type alias HoverEffect =
    { scale : Float
    , shift : Vector
    }


noEffect : HoverEffect
noEffect =
    { scale = 1, shift = ( 0, 0 ) }


hover : Maybe Position -> Cell -> HoverEffect
hover pointer ( column, row ) =
    case pointer of
        Nothing ->
            noEffect

        Just ( px, py ) ->
            let
                dx =
                    position column + spacing / 2 - px

                dy =
                    position row + spacing / 2 - py

                apart =
                    sqrt (dx * dx + dy * dy)
            in
            { scale = 1 + hoverSwell * max 0 (1 - apart / spacing)
            , shift =
                if apart == 0 || apart >= hoverReach then
                    ( 0, 0 )

                else
                    let
                        push =
                            hoverShift * sin (pi * apart / hoverReach)
                    in
                    ( dx / apart * push, dy / apart * push )
            }
