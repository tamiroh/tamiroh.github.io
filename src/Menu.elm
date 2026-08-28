module Menu exposing (Menu, glyphsGenerator, hoverStyle, view)

import Geometry exposing (Position)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Random
import Screen exposing (Screen)



-- MENU


type alias Look =
    { ink : String
    , paper : String
    , stroke : Float
    }


type alias Menu =
    { position : Position
    , items : List String
    }



-- VIEW


itemClass : String
itemClass =
    "context-menu-item"


width : Float
width =
    170


cornerRadius : Float
cornerRadius =
    10


itemHeight : Float
itemHeight =
    28


view : Look -> Screen -> msg -> Menu -> Html msg
view look screen chosen menu =
    let
        height : Float
        height =
            itemHeight * toFloat (List.length menu.items)

        ( rawX, rawY ) =
            menu.position

        x : Float
        x =
            clamp 0 (max 0 (screen.width - width)) rawX

        y : Float
        y =
            clamp 0 (max 0 (screen.height - height)) rawY

        itemView : String -> Html msg
        itemView label =
            Html.div
                [ Attr.class itemClass
                , Attr.style "height" (String.fromFloat itemHeight ++ "px")
                , Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                , Attr.style "padding" "0 12px"
                , Attr.style "white-space" "nowrap"
                , Attr.style "overflow" "hidden"
                , Html.Events.onClick chosen
                ]
                [ Html.text label ]
    in
    Html.div
        [ Attr.style "position" "fixed"
        , Attr.style "left" (String.fromFloat x ++ "px")
        , Attr.style "top" (String.fromFloat y ++ "px")
        , Attr.style "width" (String.fromFloat width ++ "px")
        , Attr.style "box-sizing" "border-box"
        , Attr.style "background" look.paper
        , Attr.style "border" (String.fromFloat look.stroke ++ "px solid " ++ look.ink)
        , Attr.style "border-radius" (String.fromFloat cornerRadius ++ "px")
        , Attr.style "font-family" "monospace"
        , Attr.style "font-size" "13px"
        , Attr.style "letter-spacing" "1px"
        , Attr.style "color" look.ink
        , Attr.style "cursor" "default"
        , Attr.style "user-select" "none"
        ]
        (List.map itemView menu.items)


hoverStyle : Look -> String
hoverStyle look =
    "." ++ itemClass ++ ":hover{background:" ++ look.ink ++ ";color:" ++ look.paper ++ "}"



-- GENERATE


itemCountRange : ( Int, Int )
itemCountRange =
    ( 4, 6 )


glyphCountRange : ( Int, Int )
glyphCountRange =
    ( 6, 14 )


glyphsGenerator : Random.Generator (List String)
glyphsGenerator =
    let
        ( fewest, most ) =
            itemCountRange
    in
    Random.int fewest most
        |> Random.andThen (\count -> Random.list count itemGenerator)


itemGenerator : Random.Generator String
itemGenerator =
    let
        ( shortest, longest ) =
            glyphCountRange
    in
    Random.int shortest longest
        |> Random.andThen (\length -> Random.list length brailleChar)
        |> Random.map String.concat


brailleChar : Random.Generator String
brailleChar =
    Random.map (\offset -> String.fromChar (Char.fromCode (0x2800 + offset))) (Random.int 0 255)
