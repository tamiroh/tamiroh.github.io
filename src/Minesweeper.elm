module Minesweeper exposing
    ( Face(..)
    , Game
    , faceOf
    , finished
    , isReady
    , new
    , reveal
    , start
    )

import Grid exposing (Cell)
import Random
import Set exposing (Set)



-- GAME


type Face
    = Hidden
    | Blank
    | Count Int
    | Mine


type Status
    = Ready
    | Playing
    | Lost
    | Won


type Game
    = Game
        { mines : Set Cell
        , revealed : Set Cell
        , status : Status
        }


mineCount : Int
mineCount =
    10



-- BUILD


new : Game
new =
    Game { mines = Set.empty, revealed = Set.empty, status = Ready }


start : Cell -> Random.Generator Game
start safe =
    Random.map (\mines -> Game { mines = mines, revealed = Set.empty, status = Playing })
        (minesGenerator safe)


minesGenerator : Cell -> Random.Generator (Set Cell)
minesGenerator safe =
    let
        candidates =
            List.filter ((/=) safe) Grid.cells
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



-- PLAY


reveal : Cell -> Game -> Game
reveal cell (Game game) =
    if Set.member cell game.mines then
        Game { game | revealed = Set.fromList Grid.cells, status = Lost }

    else
        let
            revealed =
                spread game.mines cell game.revealed
        in
        Game
            { game
                | revealed = revealed
                , status =
                    if Set.size revealed + mineCount == List.length Grid.cells then
                        Won

                    else
                        Playing
            }


spread : Set Cell -> Cell -> Set Cell -> Set Cell
spread mines cell revealed =
    if Set.member cell revealed then
        revealed

    else if minesAround mines cell == 0 then
        List.foldl (spread mines) (Set.insert cell revealed) (Grid.neighbours cell)

    else
        Set.insert cell revealed



-- QUERY


isReady : Game -> Bool
isReady (Game game) =
    game.status == Ready


finished : Game -> Bool
finished (Game game) =
    case game.status of
        Ready ->
            False

        Playing ->
            False

        Lost ->
            True

        Won ->
            True


faceOf : Cell -> Game -> Face
faceOf cell (Game game) =
    if not (Set.member cell game.revealed) then
        Hidden

    else if Set.member cell game.mines then
        Mine

    else
        case minesAround game.mines cell of
            0 ->
                Blank

            count ->
                Count count


minesAround : Set Cell -> Cell -> Int
minesAround mines cell =
    List.length (List.filter (\neighbor -> Set.member neighbor mines) (Grid.neighbours cell))
