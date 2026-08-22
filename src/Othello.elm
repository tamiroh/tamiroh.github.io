module Othello exposing (Board, Disc(..), discAt, isOver, new, play, respond, thinking)

import Dict exposing (Dict)
import Grid exposing (Cell)



-- OTHELLO


type Disc
    = Black
    | White


type Board
    = Board
        { discs : Dict Cell Disc
        , turn : Disc
        , over : Bool
        }


human : Disc
human =
    Black


computer : Disc
computer =
    White



-- BUILD


new : Board
new =
    let
        middle =
            Grid.count // 2
    in
    Board
        { discs =
            Dict.fromList
                [ ( ( middle - 1, middle - 1 ), White )
                , ( ( middle, middle ), White )
                , ( ( middle, middle - 1 ), Black )
                , ( ( middle - 1, middle ), Black )
                ]
        , turn = human
        , over = False
        }



-- PLAY


play : Cell -> Board -> Maybe Board
play cell (Board board) =
    if board.turn /= human || List.isEmpty (flips human cell board.discs) then
        Nothing

    else
        Just (settle (place human cell board.discs) computer)


respond : Board -> Board
respond (Board board) =
    if board.turn /= computer then
        Board board

    else
        case choose board.discs of
            Nothing ->
                Board board

            Just cell ->
                settle (place computer cell board.discs) human


settle : Dict Cell Disc -> Disc -> Board
settle discs next =
    let
        mine =
            legalMoves next discs

        theirs =
            legalMoves (other next) discs
    in
    Board
        { discs = discs
        , turn =
            if List.isEmpty mine then
                other next

            else
                next
        , over = List.isEmpty mine && List.isEmpty theirs
        }


place : Disc -> Cell -> Dict Cell Disc -> Dict Cell Disc
place disc cell discs =
    List.foldl (\turned -> Dict.insert turned disc)
        (Dict.insert cell disc discs)
        (flips disc cell discs)


other : Disc -> Disc
other disc =
    case disc of
        Black ->
            White

        White ->
            Black



-- RULES


flips : Disc -> Cell -> Dict Cell Disc -> List Cell
flips disc cell discs =
    if Dict.member cell discs then
        []

    else
        List.concatMap (\step -> ray disc discs step cell []) directions


ray : Disc -> Dict Cell Disc -> ( Int, Int ) -> Cell -> List Cell -> List Cell
ray disc discs ( dx, dy ) ( x, y ) found =
    let
        next =
            ( x + dx, y + dy )
    in
    if not (Grid.inside next) then
        []

    else
        case Dict.get next discs of
            Nothing ->
                []

            Just seen ->
                if seen == disc then
                    found

                else
                    ray disc discs ( dx, dy ) next (next :: found)


directions : List ( Int, Int )
directions =
    [ ( -1, -1 ), ( 0, -1 ), ( 1, -1 ), ( -1, 0 ), ( 1, 0 ), ( -1, 1 ), ( 0, 1 ), ( 1, 1 ) ]


legalMoves : Disc -> Dict Cell Disc -> List Cell
legalMoves disc discs =
    List.filter (\cell -> not (List.isEmpty (flips disc cell discs))) Grid.cells



-- QUERY


discAt : Cell -> Board -> Maybe Disc
discAt cell (Board board) =
    Dict.get cell board.discs


thinking : Board -> Bool
thinking (Board board) =
    board.turn == computer && not board.over


isOver : Board -> Bool
isOver (Board board) =
    board.over



-- COST


choose : Dict Cell Disc -> Maybe Cell
choose discs =
    legalMoves computer discs
        |> List.sortBy (\cell -> negate (cost cell discs))
        |> List.head


cost : Cell -> Dict Cell Disc -> Int
cost cell discs =
    weightAt cell * 10 + List.length (flips computer cell discs)


weightAt : Cell -> Int
weightAt ( column, row ) =
    let
        fold value =
            min value (Grid.count - 1 - value)
    in
    weights
        |> List.drop (fold row)
        |> List.head
        |> Maybe.andThen (\line -> List.head (List.drop (fold column) line))
        |> Maybe.withDefault 0


weights : List (List Int)
weights =
    [ [ 120, -20, 20, 5 ]
    , [ -20, -40, -5, -5 ]
    , [ 20, -5, 15, 3 ]
    , [ 5, -5, 3, 3 ]
    ]
