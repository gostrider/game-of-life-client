module View.Cell exposing (draw_row)

import Html exposing (Html, td, tr, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Model.Cell exposing (Cell, CellAction(..))


cell_view : Cell -> Html CellAction
cell_view cell =
    let
        css =
            style [ ( "background-color", cell.color ) ]

        action =
            Update cell.x cell.y |> onClick
    in
        td [ css, action ] [ text cell.alive ]


draw_row : List (List Cell) -> Html CellAction
draw_row cells =
    let
        rows =
            draw_row_ [] cells
    in
        Html.table [ Html.Attributes.align "center" ] rows


draw_row_ : List (Html CellAction) -> List (List Cell) -> List (Html CellAction)
draw_row_ acc cells =
    case cells of
        [] ->
            acc

        c :: cs ->
            let
                element =
                    tr [] (List.map cell_view c)
            in
                draw_row_ (element :: acc) cs
