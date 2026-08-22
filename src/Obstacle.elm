module Obstacle exposing
    ( Obstacle
    , area
    , box
    , contains
    , direction
    , edge
    , escape
    , grow
    , height
    , nearest
    , width
    )

import Geometry exposing (Position, Rect, Screen, Vector)



-- OBSTACLE


type Obstacle
    = Box Rect


type Side
    = Left
    | Right
    | Top
    | Bottom


box : Rect -> Obstacle
box =
    Box


grow : Float -> Obstacle -> Obstacle
grow margin (Box rect) =
    Box
        { left = rect.left - margin
        , top = rect.top - margin
        , right = rect.right + margin
        , bottom = rect.bottom + margin
        }



-- QUERY


width : Obstacle -> Float
width (Box rect) =
    rect.right - rect.left


height : Obstacle -> Float
height (Box rect) =
    rect.bottom - rect.top


contains : Obstacle -> Position -> Bool
contains (Box rect) ( x, y ) =
    x > rect.left && x < rect.right && y > rect.top && y < rect.bottom


nearest : Obstacle -> Position -> Position
nearest (Box rect) ( x, y ) =
    ( clamp rect.left rect.right x, clamp rect.top rect.bottom y )


area : Screen -> Obstacle -> Float
area screen (Box rect) =
    max 0 (min rect.right screen.width - max rect.left 0)
        * max 0 (min rect.bottom screen.height - max rect.top 0)



-- ESCAPE


escape : Screen -> Obstacle -> Position -> Maybe Side
escape screen obstacle point =
    let
        ( x, y ) =
            point

        (Box rect) =
            obstacle

        sideways =
            if width obstacle < screen.width then
                [ ( x - rect.left, Left ), ( rect.right - x, Right ) ]

            else
                []

        upright =
            if height obstacle < screen.height then
                [ ( y - rect.top, Top ), ( rect.bottom - y, Bottom ) ]

            else
                []
    in
    List.sortBy Tuple.first (sideways ++ upright)
        |> List.head
        |> Maybe.map Tuple.second


direction : Side -> Vector
direction side =
    case side of
        Left ->
            ( -1, 0 )

        Right ->
            ( 1, 0 )

        Top ->
            ( 0, -1 )

        Bottom ->
            ( 0, 1 )


edge : Obstacle -> Side -> Position -> Position
edge (Box rect) side ( x, y ) =
    case side of
        Left ->
            ( rect.left, y )

        Right ->
            ( rect.right, y )

        Top ->
            ( x, rect.top )

        Bottom ->
            ( x, rect.bottom )
