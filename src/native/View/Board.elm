module View.Board exposing (..)

import Html exposing (Html, div, button, br, h1, table, text)
import Html.Attributes exposing (align)
import Html.Events exposing (onClick)
import Model.Board exposing (Board, BoardAction(UpdateCell, Reset, Send))
import View.Cell exposing (draw_row)


board_view : Board -> Html BoardAction
board_view board =
    let
        cells_grid =
            draw_row board.cells |> Html.map UpdateCell
    in
        div [ align "center" ]
            [ cells_grid
            , br [] []
            , div []
                [ button [ onClick Reset ] [ text "Reset" ]
                , button [ onClick Send ] [ text "Start" ]
                , br [] []
                , br [] []
                , text board.status
                ]
            ]
