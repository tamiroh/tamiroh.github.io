module Board exposing (Content(..), Mark(..), Shade(..), size, view)

import Cursor
import Geometry exposing (Position, Screen, Vector)
import Grid exposing (Cell)
import Skull
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Svg.Events



-- BOARD


type alias Look msg =
    { ink : String
    , paper : String
    , stroke : Float
    , screen : Screen
    , pointer : Maybe Position
    , drift : Cell -> Vector
    , content : Cell -> Content
    , click : Cell -> msg
    }


type Content
    = Bare
    | Open Mark
    | Piece Shade


type Mark
    = Blank
    | Pips Int
    | Mine


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
    spacing * toFloat Grid.count + margin * 2


position : Int -> Float
position index =
    margin + spacing * toFloat index


about : Float -> Float -> Float -> String
about midX midY factor =
    String.concat
        [ "translate("
        , String.fromFloat midX
        , ","
        , String.fromFloat midY
        , ") scale("
        , String.fromFloat factor
        , ") translate("
        , String.fromFloat (negate midX)
        , ","
        , String.fromFloat (negate midY)
        , ")"
        ]



-- VIEW


view : Look msg -> Svg msg
view look =
    Svg.svg
        [ SvgAttr.width (String.fromFloat size)
        , SvgAttr.height (String.fromFloat size)
        , SvgAttr.viewBox ("0 0 " ++ String.fromFloat size ++ " " ++ String.fromFloat size)
        , SvgAttr.style "overflow: visible"
        ]
        (List.concatMap (cellView look (pointerOnBoard look.screen look.pointer)) Grid.cells)


cellView : Look msg -> Maybe Position -> Cell -> List (Svg msg)
cellView look pointer cell =
    let
        ( column, row ) =
            cell

        hovered =
            hover pointer cell

        ( shiftX, shiftY ) =
            hovered.shift

        ( driftX, driftY ) =
            look.drift cell

        seen =
            look.content cell

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
    in
    [ Svg.g
        [ SvgAttr.transform (about (x + cellSize / 2) (y + cellSize / 2) swelled)
        , SvgAttr.strokeWidth (String.fromFloat (look.stroke / swelled))
        ]
        (Svg.rect
            [ SvgAttr.x (String.fromFloat x)
            , SvgAttr.y (String.fromFloat y)
            , SvgAttr.width (String.fromFloat cellSize)
            , SvgAttr.height (String.fromFloat cellSize)
            , SvgAttr.fill
                (if opened then
                    look.ink

                 else
                    look.paper
                )
            , SvgAttr.stroke look.ink
            , SvgAttr.rx (String.fromFloat cellRadius)
            , SvgAttr.cursor (Cursor.css look.ink look.paper look.stroke True)
            , Svg.Events.onClick (look.click cell)
            ]
            []
            :: cellMarks look seen x y
        )
    ]


cellMarks : Look msg -> Content -> Float -> Float -> List (Svg msg)
cellMarks look seen x y =
    case seen of
        Bare ->
            []

        Open Blank ->
            []

        Open (Pips count) ->
            List.map (pip look x y) (pipCells count)

        Open Mine ->
            Skull.view look.ink look.paper cellSize x y

        Piece shade ->
            [ disc look shade x y ]


disc : Look msg -> Shade -> Float -> Float -> Svg msg
disc look shade x y =
    Svg.circle
        [ SvgAttr.cx (String.fromFloat (x + cellSize / 2))
        , SvgAttr.cy (String.fromFloat (y + cellSize / 2))
        , SvgAttr.r (String.fromFloat discRadius)
        , SvgAttr.fill
            (case shade of
                Dark ->
                    look.ink

                Light ->
                    look.paper
            )
        , SvgAttr.stroke look.ink
        ]
        []


pip : Look msg -> Float -> Float -> ( Int, Int ) -> Svg msg
pip look x y ( column, row ) =
    Svg.circle
        [ SvgAttr.cx (String.fromFloat (x + pipOffset column))
        , SvgAttr.cy (String.fromFloat (y + pipOffset row))
        , SvgAttr.r (String.fromFloat pipRadius)
        , SvgAttr.fill look.paper
        ]
        []



-- MARKS


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
