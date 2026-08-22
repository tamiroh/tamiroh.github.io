module Boid exposing (Boid, Rect, flock, generator, radius, wrapCopies)

import Random



-- BOID


type alias Boid =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    }


type alias Extent =
    { width : Float
    , height : Float
    }


type alias Rect =
    { left : Float
    , top : Float
    , right : Float
    , bottom : Float
    }


radius : Float
radius =
    5



-- SEED


areaPerBoid : Float
areaPerBoid =
    16000


seedAttempts : Int
seedAttempts =
    8


count : Extent -> Rect -> Int
count screen rect =
    let
        blocked =
            max 0 (min rect.right screen.width - max rect.left 0)
                * max 0 (min rect.bottom screen.height - max rect.top 0)
    in
    clamp 6 24 (round ((screen.width * screen.height - blocked) / areaPerBoid))


generator : Extent -> Rect -> Random.Generator (List Boid)
generator screen rect =
    Random.list (count screen rect) (single screen rect)


single : Extent -> Rect -> Random.Generator Boid
single screen rect =
    Random.map2
        (\( x, y ) heading ->
            { x = x
            , y = y
            , vx = cos heading * slowest
            , vy = sin heading * slowest
            }
        )
        (placeGenerator screen rect seedAttempts)
        (Random.float 0 (2 * pi))


placeGenerator : Extent -> Rect -> Int -> Random.Generator ( Float, Float )
placeGenerator screen rect attempts =
    Random.map2 Tuple.pair (Random.float 0 screen.width) (Random.float 0 screen.height)
        |> Random.andThen
            (\point ->
                if attempts <= 0 || clearOf rect point then
                    Random.constant (confine screen rect point)

                else
                    placeGenerator screen rect (attempts - 1)
            )


clearOf : Rect -> ( Float, Float ) -> Bool
clearOf rect ( x, y ) =
    let
        reach =
            boardClearance + radius
    in
    not (x > rect.left - reach && x < rect.right + reach && y > rect.top - reach && y < rect.bottom + reach)



-- SIMULATE


frameMillis : Float
frameMillis =
    1000 / 60


roomy : Extent -> Rect -> Bool
roomy screen rect =
    rect.right - rect.left < screen.width || rect.bottom - rect.top < screen.height


flock : Float -> Extent -> Rect -> Maybe ( Float, Float ) -> List Boid -> List Boid
flock delta screen rect pointer boids =
    if roomy screen rect then
        List.map (steer (min 2 (delta / frameMillis)) screen rect pointer boids) boids

    else
        boids


steer : Float -> Extent -> Rect -> Maybe ( Float, Float ) -> List Boid -> Boid -> Boid
steer dt screen rect pointer boids boid =
    let
        near =
            neighborsOf screen boid boids

        ( sx, sy ) =
            separation near

        ( ax, ay ) =
            alignment boid near

        ( hx, hy ) =
            cohesion near

        ( bx, by ) =
            avoid screen rect boid

        ( fx, fy ) =
            flee screen pointer boid

        ( vx, vy ) =
            clampSpeed
                ( boid.vx + (sx * separationWeight + ax * alignmentWeight + hx * cohesionWeight + bx * avoidWeight + fx * fleeWeight) * dt
                , boid.vy + (sy * separationWeight + ay * alignmentWeight + hy * cohesionWeight + by * avoidWeight + fy * fleeWeight) * dt
                )

        ( x, y ) =
            confine screen rect ( boid.x + vx * dt, boid.y + vy * dt )
    in
    { x = x, y = y, vx = vx, vy = vy }


slowest : Float
slowest =
    1.1


fastest : Float
fastest =
    2.2


clampSpeed : ( Float, Float ) -> ( Float, Float )
clampSpeed ( vx, vy ) =
    let
        speed =
            sqrt (vx * vx + vy * vy)
    in
    if speed > fastest then
        ( vx / speed * fastest, vy / speed * fastest )

    else if speed > 0 && speed < slowest then
        ( vx / speed * slowest, vy / speed * slowest )

    else
        ( vx, vy )



-- FLOCK


vision : Float
vision =
    60


personalSpace : Float
personalSpace =
    30


separationWeight : Float
separationWeight =
    0.5


alignmentWeight : Float
alignmentWeight =
    0.06


cohesionWeight : Float
cohesionWeight =
    0.004


type alias Neighbor =
    { boid : Boid
    , dx : Float
    , dy : Float
    , apart : Float
    }


neighborsOf : Extent -> Boid -> List Boid -> List Neighbor
neighborsOf screen boid boids =
    List.filterMap
        (\other ->
            let
                dx =
                    wrapDelta screen.width (boid.x - other.x)

                dy =
                    wrapDelta screen.height (boid.y - other.y)

                apart =
                    sqrt (dx * dx + dy * dy)
            in
            if apart > 0 && apart <= vision then
                Just { boid = other, dx = dx, dy = dy, apart = apart }

            else
                Nothing
        )
        boids


separation : List Neighbor -> ( Float, Float )
separation near =
    let
        crowd =
            List.filter (\other -> other.apart < personalSpace) near
    in
    List.foldl
        (\other ( ax, ay ) ->
            let
                push =
                    1 - other.apart / personalSpace
            in
            ( ax + other.dx / other.apart * push, ay + other.dy / other.apart * push )
        )
        ( 0, 0 )
        crowd
        |> average (List.length crowd)


alignment : Boid -> List Neighbor -> ( Float, Float )
alignment boid near =
    if List.isEmpty near then
        ( 0, 0 )

    else
        let
            ( ax, ay ) =
                List.foldl (\other ( sx, sy ) -> ( sx + other.boid.vx, sy + other.boid.vy )) ( 0, 0 ) near
                    |> average (List.length near)
        in
        ( ax - boid.vx, ay - boid.vy )


cohesion : List Neighbor -> ( Float, Float )
cohesion near =
    let
        ( hx, hy ) =
            List.foldl (\other ( sx, sy ) -> ( sx + other.dx, sy + other.dy )) ( 0, 0 ) near
                |> average (List.length near)
    in
    ( negate hx, negate hy )


average : Int -> ( Float, Float ) -> ( Float, Float )
average size ( x, y ) =
    if size == 0 then
        ( 0, 0 )

    else
        ( x / toFloat size, y / toFloat size )



-- AVOID


avoidWeight : Float
avoidWeight =
    1.6


boardClearance : Float
boardClearance =
    34


avoid : Extent -> Rect -> Boid -> ( Float, Float )
avoid screen rect boid =
    let
        dx =
            boid.x - clamp rect.left rect.right boid.x

        dy =
            boid.y - clamp rect.top rect.bottom boid.y

        apart =
            sqrt (dx * dx + dy * dy)

        reach =
            boardClearance + radius
    in
    if apart >= reach then
        ( 0, 0 )

    else if apart == 0 then
        case nearestExit screen rect boid.x boid.y of
            Just Left ->
                ( -1, 0 )

            Just Right ->
                ( 1, 0 )

            Just Top ->
                ( 0, -1 )

            Just Bottom ->
                ( 0, 1 )

            Nothing ->
                ( 0, 0 )

    else
        let
            ( ux, uy ) =
                normalize ( dx, dy )

            push =
                1 - apart / reach
        in
        ( ux * push, uy * push )


grown : Rect -> Rect
grown rect =
    { left = rect.left - radius
    , top = rect.top - radius
    , right = rect.right + radius
    , bottom = rect.bottom + radius
    }


type Side
    = Left
    | Right
    | Top
    | Bottom


nearestExit : Extent -> Rect -> Float -> Float -> Maybe Side
nearestExit screen rect x y =
    let
        sideways =
            if rect.right - rect.left < screen.width then
                [ ( x - rect.left, Left ), ( rect.right - x, Right ) ]

            else
                []

        upright =
            if rect.bottom - rect.top < screen.height then
                [ ( y - rect.top, Top ), ( rect.bottom - y, Bottom ) ]

            else
                []
    in
    List.sortBy Tuple.first (sideways ++ upright)
        |> List.head
        |> Maybe.map Tuple.second


fleeWeight : Float
fleeWeight =
    3


fleeRange : Float
fleeRange =
    120


flee : Extent -> Maybe ( Float, Float ) -> Boid -> ( Float, Float )
flee screen pointer boid =
    case pointer of
        Nothing ->
            ( 0, 0 )

        Just ( x, y ) ->
            let
                dx =
                    wrapDelta screen.width (boid.x - x)

                dy =
                    wrapDelta screen.height (boid.y - y)

                apart =
                    sqrt (dx * dx + dy * dy)
            in
            if apart == 0 || apart >= fleeRange then
                ( 0, 0 )

            else
                let
                    ( ux, uy ) =
                        normalize ( dx, dy )

                    push =
                        1 - apart / fleeRange
                in
                ( ux * push, uy * push )


normalize : ( Float, Float ) -> ( Float, Float )
normalize ( x, y ) =
    let
        length =
            sqrt (x * x + y * y)
    in
    if length == 0 then
        ( 0, 0 )

    else
        ( x / length, y / length )



-- TORUS


confine : Extent -> Rect -> ( Float, Float ) -> ( Float, Float )
confine screen rect point =
    let
        ( x, y ) =
            pushOut screen rect point
    in
    ( wrap screen.width x, wrap screen.height y )


pushOut : Extent -> Rect -> ( Float, Float ) -> ( Float, Float )
pushOut screen outer ( x, y ) =
    let
        rect =
            grown outer
    in
    if x > rect.left && x < rect.right && y > rect.top && y < rect.bottom then
        case nearestExit screen rect x y of
            Just Left ->
                ( rect.left, y )

            Just Right ->
                ( rect.right, y )

            Just Top ->
                ( x, rect.top )

            Just Bottom ->
                ( x, rect.bottom )

            Nothing ->
                ( x, y )

    else
        ( x, y )


wrap : Float -> Float -> Float
wrap span value =
    if span <= 0 then
        value

    else
        value - span * toFloat (floor (value / span))


wrapDelta : Float -> Float -> Float
wrapDelta span value =
    if value > span / 2 then
        value - span

    else if value < negate (span / 2) then
        value + span

    else
        value


wrapCopies : Extent -> Boid -> List ( Float, Float )
wrapCopies screen boid =
    let
        xs =
            boid.x :: mirror screen.width boid.x

        ys =
            boid.y :: mirror screen.height boid.y
    in
    List.concatMap (\x -> List.map (Tuple.pair x) ys) xs


mirror : Float -> Float -> List Float
mirror span value =
    if value < radius then
        [ value + span ]

    else if value > span - radius then
        [ value - span ]

    else
        []
