module Model.Cell
    exposing
        ( Cell
        , CellAction(..)
        , init
        , init_cells
        , cell_update
        , x_
        , y_
        , pos_
        , pos__
        , alive_
        , alive__
        , color_
        , color__
        )

import Utils.Utils as U


type alias Cell =
    { x : Int
    , y : Int
    , alive : String
    , color : String
    }


type CellAction
    = Update Int Int


init : Cell
init =
    Cell 0 0 "X" "grey"


init_cells : Int -> Int -> List (List Cell)
init_cells =
    init_row []


cell_update : CellAction -> Cell -> Cell
cell_update action cell =
    case action of
        Update x_ y_ ->
            let
                flip_alive =
                    U.flip "X" "O" cell.alive

                flip_color =
                    U.flip "grey" "red" cell.color
            in
                Cell x_ y_ flip_alive flip_color


init_row : List (List Cell) -> Int -> Int -> List (List Cell)
init_row acc count_x count_y =
    case count_y of
        0 ->
            acc

        _ ->
            let
                next_y =
                    count_y - 1

                element =
                    init_column [] count_x count_y
            in
                init_row (element :: acc) count_x next_y


init_column : List Cell -> Int -> Int -> List Cell
init_column acc count_x count_y =
    case count_x of
        0 ->
            acc

        _ ->
            let
                id =
                    toString count_x ++ toString count_y

                element =
                    Cell count_x count_y id "grey"
            in
                init_column (element :: acc) (count_x - 1) count_y



-- Lenses


x_ : Cell -> Int
x_ { x } =
    x


y_ : Cell -> Int
y_ { y } =
    y


pos_ : Cell -> ( Int, Int )
pos_ { x, y } =
    ( x, y )


alive_ : Cell -> String
alive_ { alive } =
    alive


color_ : Cell -> String
color_ { color } =
    color


pos__ : Int -> Int -> Cell -> Cell
pos__ x y { alive, color } =
    Cell x y alive color


alive__ : String -> Cell -> Cell
alive__ alive { x, y, color } =
    Cell x y alive color


color__ : String -> Cell -> Cell
color__ color { x, y, alive } =
    Cell x y alive color
