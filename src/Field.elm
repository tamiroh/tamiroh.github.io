module Field exposing (Field, around, blocked, expel, fits, repel, roomy)

import Geometry exposing (Position, Rect, Screen, Vector)



-- FIELD


type alias Field =
    { screen : Screen
    , objects : List Obstacle
    }


around : Screen -> List Rect -> Field
around screen rects =
    { screen = screen, objects = List.map Box rects }



-- QUERY


fits : Float -> Field -> Position -> Bool
fits margin field point =
    List.all (\object -> not (contains (grow margin object) point)) field.objects


roomy : Field -> Bool
roomy field =
    List.all
        (\object -> width object < field.screen.width || height object < field.screen.height)
        field.objects


blocked : Field -> Float
blocked field =
    List.sum (List.map (area field.screen) field.objects)



-- FORCES


repel : Float -> Field -> Position -> Vector
repel reach field point =
    List.foldl
        (\object ( ax, ay ) ->
            let
                ( px, py ) =
                    push reach field.screen object point
            in
            ( ax + px, ay + py )
        )
        ( 0, 0 )
        field.objects


push : Float -> Screen -> Obstacle -> Position -> Vector
push reach screen object point =
    let
        ( px, py ) =
            point

        ( nx, ny ) =
            nearest object point

        dx =
            px - nx

        dy =
            py - ny

        apart =
            sqrt (dx * dx + dy * dy)
    in
    if apart >= reach then
        ( 0, 0 )

    else if apart == 0 then
        escape screen object point
            |> Maybe.map direction
            |> Maybe.withDefault ( 0, 0 )

    else
        let
            spread =
                1 - apart / reach
        in
        ( dx / apart * spread, dy / apart * spread )


expel : Float -> Field -> Position -> Position
expel margin field point =
    List.foldl (shove margin field.screen) point field.objects


shove : Float -> Screen -> Obstacle -> Position -> Position
shove margin screen object point =
    let
        solid =
            grow margin object
    in
    if contains solid point then
        escape screen solid point
            |> Maybe.map (\side -> edge solid side point)
            |> Maybe.withDefault point

    else
        point



-- OBSTACLE


type Obstacle
    = Box Rect


type Side
    = Left
    | Right
    | Top
    | Bottom


grow : Float -> Obstacle -> Obstacle
grow margin (Box rect) =
    Box
        { left = rect.left - margin
        , top = rect.top - margin
        , right = rect.right + margin
        , bottom = rect.bottom + margin
        }


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
