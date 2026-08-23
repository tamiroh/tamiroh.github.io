module Field exposing (Body, Field, Obstacle, around, blocked, bounce, collide, drift, expel, fits, heart, middle, outline, repel, roomy, spin, square, triangle, wrap)

import Geometry exposing (Position, Vector)
import Millis exposing (Millis)
import Screen exposing (Screen)
import Torus exposing (Torus)



-- FIELD


type alias Field =
    { screen : Screen
    , objects : List Obstacle
    }


around : Screen -> List Obstacle -> Field
around screen objects =
    { screen = screen, objects = objects }



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
                    push reach object point
            in
            ( ax + px, ay + py )
        )
        ( 0, 0 )
        field.objects


push : Float -> Obstacle -> Position -> Vector
push reach object point =
    let
        ( px, py ) =
            point

        ( nx, ny ) =
            onEdge object point

        inside =
            contains object point

        ( dx, dy ) =
            if inside then
                ( nx - px, ny - py )

            else
                ( px - nx, py - ny )

        apart =
            sqrt (dx * dx + dy * dy)
    in
    if inside then
        unit ( dx, dy )

    else if apart == 0 || apart >= reach then
        ( 0, 0 )

    else
        let
            ( ux, uy ) =
                unit ( dx, dy )

            falloff =
                1 - apart / reach
        in
        ( ux * falloff, uy * falloff )


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
        exits solid point
            |> List.filter (onScreen screen)
            |> nearestTo point
            |> Maybe.withDefault (onEdge solid point)

    else
        point


onScreen : Screen -> Position -> Bool
onScreen screen ( x, y ) =
    x >= 0 && x <= screen.width && y >= 0 && y <= screen.height


nearestTo : Position -> List Position -> Maybe Position
nearestTo point candidates =
    List.sortBy (apartFrom point) candidates |> List.head


apartFrom : Position -> Position -> Float
apartFrom ( ax, ay ) ( bx, by ) =
    (ax - bx) ^ 2 + (ay - by) ^ 2


unit : Vector -> Vector
unit ( x, y ) =
    let
        size =
            sqrt (x * x + y * y)
    in
    if size == 0 then
        ( 0, 1 )

    else
        ( x / size, y / size )



-- OBSTACLE


type Obstacle
    = Obstacle (List Position)


square : Position -> Float -> Obstacle
square ( x, y ) span =
    Obstacle
        [ ( x - span / 2, y - span / 2 )
        , ( x + span / 2, y - span / 2 )
        , ( x + span / 2, y + span / 2 )
        , ( x - span / 2, y + span / 2 )
        ]


triangle : Position -> Float -> Obstacle
triangle ( x, y ) span =
    Obstacle
        [ ( x, y - span / 2 )
        , ( x + span / 2, y + span / 2 )
        , ( x - span / 2, y + span / 2 )
        ]


heart : Position -> Float -> Obstacle
heart ( x, y ) span =
    let
        scale =
            span / 34

        at turn =
            let
                t =
                    turn * 2 * pi / heartSteps
            in
            ( x + scale * 16 * (sin t ^ 3)
            , y - scale * (13 * cos t - 5 * cos (2 * t) - 2 * cos (3 * t) - cos (4 * t))
            )
    in
    Obstacle (List.map at (List.map toFloat (List.range 0 (round heartSteps - 1))))


heartSteps : Float
heartSteps =
    24


outline : Obstacle -> List Position
outline (Obstacle points) =
    points


middle : Obstacle -> Position
middle (Obstacle points) =
    let
        count =
            toFloat (List.length points)
    in
    ( List.sum (List.map Tuple.first points) / count
    , List.sum (List.map Tuple.second points) / count
    )


grow : Float -> Obstacle -> Obstacle
grow margin object =
    let
        ( mx, my ) =
            middle object
    in
    Obstacle
        (List.map
            (\( x, y ) ->
                let
                    ( ux, uy ) =
                        unit ( x - mx, y - my )
                in
                ( x + ux * margin, y + uy * margin )
            )
            (outline object)
        )


spinRate : Float
spinRate =
    pi / 3


spin : Millis -> Obstacle -> Obstacle
spin delta object =
    let
        angle =
            spinRate * delta / 1000

        ( mx, my ) =
            middle object

        c =
            cos angle

        s =
            sin angle
    in
    Obstacle
        (List.map
            (\( x, y ) ->
                let
                    dx =
                        x - mx

                    dy =
                        y - my
                in
                ( mx + dx * c - dy * s, my + dx * s + dy * c )
            )
            (outline object)
        )


width : Obstacle -> Float
width object =
    spread (List.map Tuple.first (outline object))


height : Obstacle -> Float
height object =
    spread (List.map Tuple.second (outline object))


spread : List Float -> Float
spread values =
    Maybe.withDefault 0 (List.maximum values) - Maybe.withDefault 0 (List.minimum values)


contains : Obstacle -> Position -> Bool
contains object ( x, y ) =
    List.foldl
        (\( ( ax, ay ), ( bx, by ) ) hit ->
            if (ay > y) /= (by > y) && x < (bx - ax) * (y - ay) / (by - ay) + ax then
                not hit

            else
                hit
        )
        False
        (edges object)


edges : Obstacle -> List ( Position, Position )
edges object =
    let
        points =
            outline object
    in
    List.map2 Tuple.pair points (List.drop 1 points ++ List.take 1 points)


onEdge : Obstacle -> Position -> Position
onEdge object point =
    Maybe.withDefault point (nearestTo point (exits object point))


exits : Obstacle -> Position -> List Position
exits object point =
    List.map (\( a, b ) -> alongside a b point) (edges object)


alongside : Position -> Position -> Position -> Position
alongside ( ax, ay ) ( bx, by ) ( px, py ) =
    let
        dx =
            bx - ax

        dy =
            by - ay

        len =
            dx * dx + dy * dy
    in
    if len == 0 then
        ( ax, ay )

    else
        let
            t =
                clamp 0 1 (((px - ax) * dx + (py - ay) * dy) / len)
        in
        ( ax + dx * t, ay + dy * t )


area : Screen -> Obstacle -> Float
area screen object =
    let
        xs =
            List.map Tuple.first (outline object)

        ys =
            List.map Tuple.second (outline object)

        clipped low high limit =
            max 0 (min (Maybe.withDefault 0 high) limit - max (Maybe.withDefault 0 low) 0)
    in
    clipped (List.minimum xs) (List.maximum xs) screen.width
        * clipped (List.minimum ys) (List.maximum ys) screen.height


translate : Vector -> Obstacle -> Obstacle
translate ( dx, dy ) object =
    Obstacle (List.map (\( x, y ) -> ( x + dx, y + dy )) (outline object))



-- BODY


type alias Body =
    { shape : Obstacle
    , velocity : Vector
    }


bodyReach : Body -> Float
bodyReach body =
    max (width body.shape) (height body.shape) / 2


drift : Millis -> Body -> Body
drift delta body =
    let
        ( vx, vy ) =
            body.velocity
    in
    { body | shape = translate ( vx * delta / 1000, vy * delta / 1000 ) body.shape }


bounce : Screen -> Obstacle -> Body -> Body
bounce screen board body =
    let
        center =
            middle body.shape

        ( cx, cy ) =
            center

        ( ex, ey ) =
            expel (bodyReach body) (around screen [ board ]) center

        ( dx, dy ) =
            ( ex - cx, ey - cy )
    in
    if dx == 0 && dy == 0 then
        body

    else
        let
            ( nx, ny ) =
                unit ( dx, dy )

            ( vx, vy ) =
                body.velocity

            vn =
                vx * nx + vy * ny
        in
        { shape = translate ( dx, dy ) body.shape
        , velocity =
            if vn < 0 then
                ( vx - 2 * vn * nx, vy - 2 * vn * ny )

            else
                body.velocity
        }


wrap : Torus -> Body -> Body
wrap torus body =
    let
        center =
            middle body.shape

        ( cx, cy ) =
            center

        ( wx, wy ) =
            Torus.wrap torus center

        ( dx, dy ) =
            ( wx - cx, wy - cy )
    in
    if dx == 0 && dy == 0 then
        body

    else
        { body | shape = translate ( dx, dy ) body.shape }


collide : List Body -> List Body
collide bodies =
    let
        indexed =
            List.indexedMap Tuple.pair bodies
    in
    List.map (\( i, body ) -> List.foldl (bounceOff i) body indexed) indexed


bounceOff : Int -> ( Int, Body ) -> Body -> Body
bounceOff self ( index, other ) body =
    if index == self then
        body

    else
        let
            ( cx, cy ) =
                middle body.shape

            ( ox, oy ) =
                middle other.shape

            dx =
                cx - ox

            dy =
                cy - oy

            apart =
                sqrt (dx * dx + dy * dy)
        in
        if apart == 0 || apart >= bodyReach body + bodyReach other then
            body

        else
            let
                ( nx, ny ) =
                    ( dx / apart, dy / apart )

                ( vx, vy ) =
                    body.velocity

                ( ovx, ovy ) =
                    other.velocity

                vn =
                    vx * nx + vy * ny

                ovn =
                    ovx * nx + ovy * ny
            in
            if vn - ovn >= 0 then
                body

            else
                { body | velocity = ( vx - vn * nx + ovn * nx, vy - vn * ny + ovn * ny ) }
