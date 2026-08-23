module Grid exposing (Cell, cells, inside, neighbours, side)

-- GRID


type alias Cell =
    ( Int, Int )


side : Int
side =
    8


cells : List Cell
cells =
    let
        indices =
            List.range 0 (side - 1)
    in
    List.concatMap (\column -> List.map (Tuple.pair column) indices) indices


inside : Cell -> Bool
inside ( column, row ) =
    column >= 0 && column < side && row >= 0 && row < side


neighbours : Cell -> List Cell
neighbours ( column, row ) =
    List.concatMap
        (\columnOffset ->
            List.map (\rowOffset -> ( column + columnOffset, row + rowOffset )) [ -1, 0, 1 ]
        )
        [ -1, 0, 1 ]
        |> List.filter (\cell -> cell /= ( column, row ) && inside cell)
