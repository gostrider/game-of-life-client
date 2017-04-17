module View.Board exposing (board_view)

import List exposing (map)
import Html exposing (Html, div, button, br, h1, table, text)
import Html.Attributes exposing (align)
import Html.Events exposing (onClick)
import Model.Board
    exposing
        ( Board
        , BoardAction
            ( UpdateCell
            , ResetInput
            , Send
            )
        )
import View.Cell exposing (draw_cells)


board_view : Board -> Html BoardAction
board_view board =
    let
        cells_grid =
            draw_cells 5 5 board.pending

        lift_grid =
            cells_grid |> map (Html.map UpdateCell)
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
