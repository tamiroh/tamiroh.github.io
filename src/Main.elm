module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Svg.Events



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    Set ( Int, Int )


init : Model
init =
    Set.empty



-- UPDATE


type Msg
    = Clicked ( Int, Int )


update : Msg -> Model -> Model
update msg model =
    case msg of
        Clicked cell ->
            Set.insert cell model



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



-- BOARD


cellCount : Int
cellCount =
    8


spacing : Float
spacing =
    48


margin : Float
margin =
    36


boardSize : Float
boardSize =
    spacing * toFloat cellCount + margin * 2


lineIndices : List Int
lineIndices =
    List.range 0 cellCount


cells : List ( Int, Int )
cells =
    let
        cellIndices =
            List.range 0 (cellCount - 1)
    in
    List.concatMap (\column -> List.map (Tuple.pair column) cellIndices) cellIndices


ink : String
ink =
    "#363636"


position : Int -> Float
position index =
    margin + spacing * toFloat index


board : Model -> Svg Msg
board model =
    Svg.svg
        [ SvgAttr.width (String.fromFloat boardSize)
        , SvgAttr.height (String.fromFloat boardSize)
        , SvgAttr.viewBox ("0 0 " ++ String.fromFloat boardSize ++ " " ++ String.fromFloat boardSize)
        ]
        (filledCells model ++ gridLines ++ clickTargets)


filledCells : Model -> List (Svg msg)
filledCells model =
    List.map (\cell -> cellRect cell [ SvgAttr.fill ink ]) (Set.toList model)


clickTargets : List (Svg Msg)
clickTargets =
    List.map
        (\cell ->
            cellRect cell
                [ SvgAttr.fill "transparent"
                , SvgAttr.cursor "pointer"
                , Svg.Events.onClick (Clicked cell)
                ]
        )
        cells


cellRect : ( Int, Int ) -> List (Svg.Attribute msg) -> Svg msg
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
        lineIndices


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
