module View.Cell exposing (draw_cells)

import Html exposing (Html, td, tr, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Model.Cell exposing (Cell, CellAction(..), alive__, color__)
import List exposing (member)


cell_view : Cell -> Html CellAction
cell_view cell =
    let
        css =
            style [ ( "background-color", cell.color ) ]

        action =
            Update cell.x cell.y |> onClick
    in
        td [ css, action ] [ text cell.alive ]


draw_cells : Int -> Int -> List Cell -> List (Html CellAction)
draw_cells =
    draw_row []


draw_row : List (Html CellAction) -> Int -> Int -> List Cell -> List (Html CellAction)
draw_row rows row column cells =
    let
        next =
            row - 1

        cell_y =
            flip Cell row

        element =
            draw_column [] column cell_y cells |> tr []
    in
        case row of
            0 ->
                rows

            _ ->
                draw_row (element :: rows) next column cells


draw_column : List (Html CellAction) -> Int -> (Int -> String -> String -> Cell) -> List Cell -> List (Html CellAction)
draw_column columns column cell cells =
    let
        next =
            column - 1

        element =
            (cell column "X" "grey") |> cell_view << flip transform_cell cells
    in
        case column of
            0 ->
                columns

            _ ->
                draw_column (element :: columns) next cell cells


transform_cell : Cell -> List Cell -> Cell
transform_cell cell cells =
    if member cell cells then
        cell |> color__ "red" << alive__ "O"
    else
        cell
