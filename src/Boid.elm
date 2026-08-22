module Boid exposing (Boid, flock, generator, wrapCopies)

import Field exposing (Field)
import Geometry exposing (Position, Screen, Vector, wrap, wrapDelta)
import Random



-- BOID


type alias Boid =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
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


count : Field -> Int
count field =
    clamp 6
        24
        (round ((field.screen.width * field.screen.height - Field.blocked field) / areaPerBoid))


generator : Field -> Random.Generator (List Boid)
generator field =
    Random.list (count field) (single field)


single : Field -> Random.Generator Boid
single field =
    Random.map2
        (\( x, y ) heading ->
            { x = x
            , y = y
            , vx = cos heading * slowest
            , vy = sin heading * slowest
            }
        )
        (placeGenerator field seedAttempts)
        (Random.float 0 (2 * pi))


placeGenerator : Field -> Int -> Random.Generator Position
placeGenerator field attempts =
    Random.map2 Tuple.pair (Random.float 0 field.screen.width) (Random.float 0 field.screen.height)
        |> Random.andThen
            (\point ->
                if attempts <= 0 || Field.fits (clearance + radius) field point then
                    Random.constant (confine field point)

                else
                    placeGenerator field (attempts - 1)
            )



-- SIMULATE


frameMillis : Float
frameMillis =
    1000 / 60


flock : Float -> Field -> Maybe Position -> List Boid -> List Boid
flock delta field pointer boids =
    if Field.roomy field then
        List.map (steer (min 2 (delta / frameMillis)) field pointer boids) boids

    else
        boids


steer : Float -> Field -> Maybe Position -> List Boid -> Boid -> Boid
steer dt field pointer boids boid =
    let
        near =
            neighborsOf field.screen boid boids

        ( sx, sy ) =
            separation near

        ( ax, ay ) =
            alignment boid near

        ( hx, hy ) =
            cohesion near

        ( bx, by ) =
            Field.repel (clearance + radius) field ( boid.x, boid.y )

        ( fx, fy ) =
            flee field.screen pointer boid

        ( vx, vy ) =
            clampSpeed
                ( boid.vx + (sx * separationWeight + ax * alignmentWeight + hx * cohesionWeight + bx * avoidWeight + fx * fleeWeight) * dt
                , boid.vy + (sy * separationWeight + ay * alignmentWeight + hy * cohesionWeight + by * avoidWeight + fy * fleeWeight) * dt
                )

        ( x, y ) =
            confine field ( boid.x + vx * dt, boid.y + vy * dt )
    in
    { x = x, y = y, vx = vx, vy = vy }


slowest : Float
slowest =
    1.1


fastest : Float
fastest =
    2.2


clampSpeed : Vector -> Vector
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


neighborsOf : Screen -> Boid -> List Boid -> List Neighbor
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


separation : List Neighbor -> Vector
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


alignment : Boid -> List Neighbor -> Vector
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


cohesion : List Neighbor -> Vector
cohesion near =
    let
        ( hx, hy ) =
            List.foldl (\other ( sx, sy ) -> ( sx + other.dx, sy + other.dy )) ( 0, 0 ) near
                |> average (List.length near)
    in
    ( negate hx, negate hy )


average : Int -> Vector -> Vector
average size ( x, y ) =
    if size == 0 then
        ( 0, 0 )

    else
        ( x / toFloat size, y / toFloat size )



-- AVOID


avoidWeight : Float
avoidWeight =
    1.6


clearance : Float
clearance =
    34


fleeWeight : Float
fleeWeight =
    3


fleeRange : Float
fleeRange =
    120


flee : Screen -> Maybe Position -> Boid -> Vector
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


normalize : Vector -> Vector
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


confine : Field -> Position -> Position
confine field point =
    let
        ( x, y ) =
            Field.expel radius field point
    in
    ( wrap field.screen.width x, wrap field.screen.height y )


wrapCopies : Screen -> Boid -> List Position
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
