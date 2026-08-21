module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr



-- MAIN


main : Program () () ()
main =
    Browser.sandbox
        { init = ()
        , update = \_ model -> model
        , view = view
        }



-- VIEW


view : () -> Html ()
view _ =
    Html.div
        [ Attr.style "display" "flex"
        , Attr.style "justify-content" "center"
        , Attr.style "align-items" "center"
        , Attr.style "min-height" "100vh"
        ]
        [ board ]



-- BOARD


lineCount : Int
lineCount =
    9


spacing : Float
spacing =
    48


margin : Float
margin =
    36


boardSize : Float
boardSize =
    spacing * toFloat (lineCount - 1) + margin * 2


starPoints : List ( Int, Int )
starPoints =
    [ ( 2, 2 ), ( 2, 6 ), ( 6, 2 ), ( 6, 6 ), ( 4, 4 ) ]


position : Int -> Float
position index =
    margin + spacing * toFloat index


board : Svg ()
board =
    Svg.svg
        [ SvgAttr.width (String.fromFloat boardSize)
        , SvgAttr.height (String.fromFloat boardSize)
        , SvgAttr.viewBox ("0 0 " ++ String.fromFloat boardSize ++ " " ++ String.fromFloat boardSize)
        ]
        (gridLines ++ stars)


gridLines : List (Svg ())
gridLines =
    List.concatMap
        (\index ->
            [ line (position 0) (position index) (position (lineCount - 1)) (position index)
            , line (position index) (position 0) (position index) (position (lineCount - 1))
            ]
        )
        (List.range 0 (lineCount - 1))


stars : List (Svg ())
stars =
    List.map
        (\( column, row ) ->
            Svg.circle
                [ SvgAttr.cx (String.fromFloat (position column))
                , SvgAttr.cy (String.fromFloat (position row))
                , SvgAttr.r "4"
                , SvgAttr.fill "#3a2c1a"
                ]
                []
        )
        starPoints


line : Float -> Float -> Float -> Float -> Svg ()
line x1 y1 x2 y2 =
    Svg.line
        [ SvgAttr.x1 (String.fromFloat x1)
        , SvgAttr.y1 (String.fromFloat y1)
        , SvgAttr.x2 (String.fromFloat x2)
        , SvgAttr.y2 (String.fromFloat y2)
        , SvgAttr.stroke "#3a2c1a"
        , SvgAttr.strokeWidth "1.5"
        ]
        []
