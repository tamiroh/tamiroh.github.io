module Rigid exposing (Body, fromStroke, path, step)

import Array exposing (Array)
import Geometry exposing (Position, Screen)



-- RIGID


type Body
    = Body
        { x : Float
        , y : Float
        , vx : Float
        , vy : Float
        , angle : Float
        , spin : Float
        , local : List Position
        , mass : Float
        , inertia : Float
        , reach : Float
        }


radius : Float
radius =
    6


gravity : Float
gravity =
    0.6


restitution : Float
restitution =
    0.15


friction : Float
friction =
    0.35


iterations : Int
iterations =
    6


slop : Float
slop =
    0.5


correction : Float
correction =
    0.4


damping : Float
damping =
    0.995


spinDamping : Float
spinDamping =
    0.97


sleeping : Float
sleeping =
    0.02


frameMillis : Float
frameMillis =
    1000 / 60



-- BUILD


fromStroke : List Position -> Maybe Body
fromStroke points =
    let
        chain =
            resample points

        size =
            List.length chain
    in
    if size < 2 then
        Nothing

    else
        let
            count =
                toFloat size

            midX =
                List.sum (List.map Tuple.first chain) / count

            midY =
                List.sum (List.map Tuple.second chain) / count

            local =
                List.map (\( x, y ) -> ( x - midX, y - midY )) chain

            spread =
                List.sum (List.map (\( x, y ) -> x * x + y * y) local)
        in
        Just
            (Body
                { x = midX
                , y = midY
                , vx = 0
                , vy = 0
                , angle = 0
                , spin = 0
                , local = local
                , mass = count
                , inertia = max 1 spread
                , reach = List.foldl (\p acc -> max acc (length p)) 0 local + radius
                }
            )


resample : List Position -> List Position
resample points =
    case points of
        [] ->
            []

        first :: rest ->
            List.reverse (List.foldl walk [ first ] rest)


walk : Position -> List Position -> List Position
walk target chain =
    case chain of
        [] ->
            [ target ]

        last :: _ ->
            let
                ( lx, ly ) =
                    last

                ( tx, ty ) =
                    target

                gap =
                    length ( tx - lx, ty - ly )
            in
            if gap < radius then
                chain

            else
                walk target (( lx + (tx - lx) * radius / gap, ly + (ty - ly) * radius / gap ) :: chain)



-- SIMULATE


step : Float -> Screen -> List Body -> List Body
step delta screen bodies =
    let
        dt =
            min 2 (delta / frameMillis)
    in
    List.map (integrate dt) bodies
        |> Array.fromList
        |> resolve screen
        |> Array.toList


integrate : Float -> Body -> Body
integrate dt (Body body) =
    let
        vy =
            (body.vy + gravity * dt) * damping

        vx =
            body.vx * damping

        spin =
            body.spin * spinDamping

        asleep =
            abs vx < sleeping && abs vy < sleeping && abs spin < sleeping / 20
    in
    if asleep then
        Body { body | vx = 0, vy = 0, spin = 0 }

    else
        Body
            { body
                | vx = vx
                , vy = vy
                , spin = spin
                , x = body.x + vx * dt
                , y = body.y + vy * dt
                , angle = body.angle + spin * dt
            }


resolve : Screen -> Array Body -> Array Body
resolve screen bodies =
    List.foldl (\_ acc -> pass screen acc) bodies (List.range 1 iterations)


pass : Screen -> Array Body -> Array Body
pass screen bodies =
    let
        walls =
            Array.map (againstWalls screen) bodies
    in
    List.foldl againstEach walls (couples (Array.length walls))


couples : Int -> List ( Int, Int )
couples size =
    List.range 0 (size - 1)
        |> List.concatMap (\i -> List.map (Tuple.pair i) (List.range (i + 1) (size - 1)))



-- WALLS


againstWalls : Screen -> Body -> Body
againstWalls screen body =
    List.foldl (touchWalls screen) body (path body)


touchWalls : Screen -> Position -> Body -> Body
touchWalls screen point body =
    let
        ( px, py ) =
            point
    in
    body
        |> whenDeep point ( 0, -1 ) (py + radius - screen.height)
        |> whenDeep point ( 1, 0 ) (radius - px)
        |> whenDeep point ( -1, 0 ) (px + radius - screen.width)


whenDeep : Position -> Position -> Float -> Body -> Body
whenDeep point normal depth body =
    if depth <= 0 then
        body

    else
        body
            |> bounce point normal
            |> separate normal depth


bounce : Position -> Position -> Body -> Body
bounce point normal body =
    let
        ( nx, ny ) =
            normal

        ( vx, vy ) =
            velocityAt point body

        approach =
            vx * nx + vy * ny
    in
    if approach >= 0 then
        body

    else
        let
            share =
                1 / massAt point normal body

            impulse =
                negate (1 + restitution) * approach * share

            pushed =
                shove point ( impulse * nx, impulse * ny ) body

            ( tx, ty ) =
                ( negate ny, nx )

            ( sx, sy ) =
                velocityAt point pushed

            skid =
                sx * tx + sy * ty

            rub =
                clamp (negate (friction * impulse)) (friction * impulse) (negate skid / massAt point ( tx, ty ) pushed)
        in
        shove point ( rub * tx, rub * ty ) pushed


separate : Position -> Float -> Body -> Body
separate ( nx, ny ) depth (Body body) =
    if depth <= slop then
        Body body

    else
        let
            push =
                (depth - slop) * correction
        in
        Body { body | x = body.x + nx * push, y = body.y + ny * push }



-- BODIES


againstEach : ( Int, Int ) -> Array Body -> Array Body
againstEach ( i, j ) bodies =
    case ( Array.get i bodies, Array.get j bodies ) of
        ( Just left, Just right ) ->
            if far left right then
                bodies

            else
                let
                    ( nextLeft, nextRight ) =
                        List.foldl overlap ( left, right ) (List.concatMap (\a -> List.map (Tuple.pair a) (path right)) (path left))
                in
                bodies |> Array.set i nextLeft |> Array.set j nextRight

        _ ->
            bodies


far : Body -> Body -> Bool
far (Body a) (Body b) =
    length ( a.x - b.x, a.y - b.y ) > a.reach + b.reach


overlap : ( Position, Position ) -> ( Body, Body ) -> ( Body, Body )
overlap ( ( ax, ay ), ( bx, by ) ) ( left, right ) =
    let
        gap =
            length ( ax - bx, ay - by )
    in
    if gap == 0 || gap >= 2 * radius then
        ( left, right )

    else
        let
            normal =
                ( (ax - bx) / gap, (ay - by) / gap )

            depth =
                2 * radius - gap

            ( nx, ny ) =
                normal

            ( lvx, lvy ) =
                velocityAt ( ax, ay ) left

            ( rvx, rvy ) =
                velocityAt ( bx, by ) right

            approach =
                (lvx - rvx) * nx + (lvy - rvy) * ny

            share =
                1 / (massAt ( ax, ay ) normal left + massAt ( bx, by ) normal right)

            impulse =
                if approach < 0 then
                    negate (1 + restitution) * approach * share

                else
                    0

            nudge =
                if depth > slop then
                    (depth - slop) * correction * 0.5

                else
                    0
        in
        ( left
            |> shove ( ax, ay ) ( impulse * nx, impulse * ny )
            |> slide ( nx * nudge, ny * nudge )
        , right
            |> shove ( bx, by ) ( negate impulse * nx, negate impulse * ny )
            |> slide ( negate nx * nudge, negate ny * nudge )
        )


slide : Position -> Body -> Body
slide ( dx, dy ) (Body body) =
    Body { body | x = body.x + dx, y = body.y + dy }



-- MECHANICS


path : Body -> List Position
path (Body body) =
    let
        turn =
            cos body.angle

        tilt =
            sin body.angle
    in
    List.map
        (\( x, y ) -> ( body.x + x * turn - y * tilt, body.y + x * tilt + y * turn ))
        body.local


velocityAt : Position -> Body -> Position
velocityAt ( px, py ) (Body body) =
    ( body.vx - body.spin * (py - body.y)
    , body.vy + body.spin * (px - body.x)
    )


massAt : Position -> Position -> Body -> Float
massAt ( px, py ) ( nx, ny ) (Body body) =
    let
        lever =
            (px - body.x) * ny - (py - body.y) * nx
    in
    1 / body.mass + lever * lever / body.inertia


shove : Position -> Position -> Body -> Body
shove ( px, py ) ( jx, jy ) (Body body) =
    Body
        { body
            | vx = body.vx + jx / body.mass
            , vy = body.vy + jy / body.mass
            , spin = body.spin + ((px - body.x) * jy - (py - body.y) * jx) / body.inertia
        }


length : Position -> Float
length ( x, y ) =
    sqrt (x * x + y * y)
