module View.Board exposing (..)

import List as L
import Html exposing (Html, div, button, br, h1, table, text)
import Html.Attributes exposing (align)
import Html.Events exposing (onClick)
import Model.Board exposing (Board, BoardAction(..))
import View.Cell exposing (draw_cells)


view : Board -> Html BoardAction
view board =
    let
        cells_grid =
            draw_cells 5 5 board.pending

        lift_grid =
            cells_grid |> L.map (Html.map UpdateCell)
    in
        div [ align "center" ]
            [ h1 [] [ text "title" ]
            , table [] lift_grid
            , br [] []
            , div []
                [ button [ onClick ResetInput ] [ text "Reset" ]
                , button [ onClick Send ] [ text "Start" ]
                , br [] []
                , br [] []
                , text board.status
                ]
            ]
