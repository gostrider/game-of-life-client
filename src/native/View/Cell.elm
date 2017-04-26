module View.Cell exposing (..)

import Html exposing (Html, td, tr, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Matrix as M
import Array as A


-- Application modules

import Model.Cell as C exposing (Cell, CellAction(..))
import Model.Board exposing (CellMatrix)


cell_view : Cell -> Html CellAction
cell_view cell =
    let
        css =
            style [ ( "background-color", Tuple.first cell.color ) ]

        action =
            onClick
                (Update (M.row cell.position) (M.col cell.position))
    in
        td [ css, action ] [ cell.alive |> text << Tuple.first ]


draw_row : CellMatrix -> Html CellAction
draw_row cells =
    let
        rows =
            draw_row_ cells
    in
        Html.table [ Html.Attributes.align "center" ] rows


draw_row_ : CellMatrix -> List (Html CellAction)
draw_row_ cells =
    let
        by cell =
            (::) (tr [] (A.map cell_view cell |> A.toList))
    in
        A.foldl by [] cells
