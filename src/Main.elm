module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Random
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Svg.Events



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
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


type alias Model =
    { mines : Set Cell
    , revealed : Set Cell
    , status : Status
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { mines = Set.empty, revealed = Set.empty, status = Ready }, Cmd.none )



-- UPDATE


type Msg
    = Clicked Cell
    | MinesPlaced Cell (Set Cell)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clicked cell ->
            case model.status of
                Ready ->
                    ( model, Random.generate (MinesPlaced cell) (mineGenerator cell) )

                Playing ->
                    ( reveal cell model, Cmd.none )

                Lost ->
                    ( model, Cmd.none )

                Won ->
                    ( model, Cmd.none )

        MinesPlaced cell mines ->
            ( reveal cell { model | mines = mines, status = Playing }, Cmd.none )


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



-- VIEW


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.style "display" "flex"
        , Attr.style "justify-content" "center"
        , Attr.style "align-items" "center"
        , Attr.style "min-height" "100vh"
        ]
        [ board model ]


board : Model -> Svg Msg
board model =
    Svg.svg
        [ SvgAttr.width (String.fromFloat boardSize)
        , SvgAttr.height (String.fromFloat boardSize)
        , SvgAttr.viewBox ("0 0 " ++ String.fromFloat boardSize ++ " " ++ String.fromFloat boardSize)
        ]
        (cellShapes model ++ gridLines ++ clickTargets model)


cellShapes : Model -> List (Svg msg)
cellShapes model =
    case model.status of
        Lost ->
            List.map (\cell -> cellRect cell [ SvgAttr.fill ink ]) cells

        _ ->
            List.concatMap (openedCell model) cells


openedCell : Model -> Cell -> List (Svg msg)
openedCell model cell =
    if Set.member cell model.revealed then
        cellRect cell [ SvgAttr.fill ink ]
            :: (case adjacentMines model.mines cell of
                    0 ->
                        []

                    count ->
                        [ label cell count ]
               )

    else
        []


label : Cell -> Int -> Svg msg
label ( column, row ) count =
    Svg.text_
        [ SvgAttr.x (String.fromFloat (position column + spacing / 2))
        , SvgAttr.y (String.fromFloat (position row + spacing / 2))
        , SvgAttr.textAnchor "middle"
        , SvgAttr.dominantBaseline "central"
        , SvgAttr.fontFamily "monospace"
        , SvgAttr.fontSize (String.fromFloat (spacing / 2))
        , SvgAttr.fill paper
        ]
        [ Svg.text (String.fromInt count) ]


clickTargets : Model -> List (Svg Msg)
clickTargets model =
    case model.status of
        Lost ->
            []

        Won ->
            []

        _ ->
            List.filterMap
                (\cell ->
                    if Set.member cell model.revealed then
                        Nothing

                    else
                        Just
                            (cellRect cell
                                [ SvgAttr.fill "transparent"
                                , SvgAttr.cursor "pointer"
                                , Svg.Events.onClick (Clicked cell)
                                ]
                            )
                )
                cells



-- BOARD


cellCount : Int
cellCount =
    8


mineCount : Int
mineCount =
    10


spacing : Float
spacing =
    48


margin : Float
margin =
    36


ink : String
ink =
    "#363636"


paper : String
paper =
    "#ffffff"


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


cellRect : Cell -> List (Svg.Attribute msg) -> Svg msg
cellRect ( column, row ) attributes =
    Svg.rect
        (SvgAttr.x (String.fromFloat (position column))
            :: SvgAttr.y (String.fromFloat (position row))
            :: SvgAttr.width (String.fromFloat spacing)
            :: SvgAttr.height (String.fromFloat spacing)
            :: attributes
        )
        []


gridLines : List (Svg msg)
gridLines =
    List.concatMap
        (\index ->
            [ line (position 0) (position index) (position cellCount) (position index)
            , line (position index) (position 0) (position index) (position cellCount)
            ]
        )
        (List.range 0 cellCount)


line : Float -> Float -> Float -> Float -> Svg msg
line x1 y1 x2 y2 =
    Svg.line
        [ SvgAttr.x1 (String.fromFloat x1)
        , SvgAttr.y1 (String.fromFloat y1)
        , SvgAttr.x2 (String.fromFloat x2)
        , SvgAttr.y2 (String.fromFloat y2)
        , SvgAttr.stroke ink
        , SvgAttr.strokeWidth "2"
        , SvgAttr.shapeRendering "crispEdges"
        ]
        []
