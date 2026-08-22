module Minesweeper exposing
    ( Cell
    , Game
    , adjacentMines
    , cellCount
    , cells
    , finished
    , isReady
    , isRevealed
    , new
    , reveal
    , start
    )

import Random
import Set exposing (Set)



-- MINESWEEPER


type alias Cell =
    ( Int, Int )


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


cellCount : Int
cellCount =
    8


mineCount : Int
mineCount =
    10


cells : List Cell
cells =
    let
        cellIndices =
            List.range 0 (cellCount - 1)
    in
    List.concatMap (\column -> List.map (Tuple.pair column) cellIndices) cellIndices


new : Game
new =
    Game { mines = Set.empty, revealed = Set.empty, status = Ready }


start : Cell -> Random.Generator Game
start safe =
    Random.map (\mines -> Game { mines = mines, revealed = Set.empty, status = Playing })
        (minesGenerator safe)


reveal : Cell -> Game -> Game
reveal cell (Game game) =
    if Set.member cell game.mines then
        Game { game | status = Lost }

    else
        let
            revealed =
                spread game.mines cell game.revealed
        in
        Game
            { game
                | revealed = revealed
                , status =
                    if Set.size revealed + mineCount == List.length cells then
                        Won

                    else
                        Playing
            }


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


isRevealed : Cell -> Game -> Bool
isRevealed cell (Game game) =
    Set.member cell game.revealed


adjacentMines : Cell -> Game -> Int
adjacentMines cell (Game game) =
    minesAround game.mines cell


minesGenerator : Cell -> Random.Generator (Set Cell)
minesGenerator safe =
    let
        candidates =
            List.filter ((/=) safe) cells
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


spread : Set Cell -> Cell -> Set Cell -> Set Cell
spread mines cell revealed =
    if Set.member cell revealed then
        revealed

    else if minesAround mines cell == 0 then
        List.foldl (spread mines) (Set.insert cell revealed) (neighbors cell)

    else
        Set.insert cell revealed


minesAround : Set Cell -> Cell -> Int
minesAround mines cell =
    List.length (List.filter (\neighbor -> Set.member neighbor mines) (neighbors cell))


neighbors : Cell -> List Cell
neighbors ( column, row ) =
    List.concatMap
        (\columnOffset ->
            List.map (\rowOffset -> ( column + columnOffset, row + rowOffset )) [ -1, 0, 1 ]
        )
        [ -1, 0, 1 ]
        |> List.filter (\cell -> cell /= ( column, row ) && List.member cell cells)
