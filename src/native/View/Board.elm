module View.Board exposing (board_view)

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
import View.Cell exposing (draw_row)


board_view : Board -> Html BoardAction
board_view board =
    let
        cells_grid =
            draw_row board.cells |> Html.map UpdateCell

    in
        div [ align "center" ]
            [ h1 [] [ text "title" ]
            , cells_grid
            , br [] []
            , div []
                [ button [ onClick ResetInput ] [ text "Reset" ]
                , button [ onClick Send ] [ text "Start" ]
                , br [] []
                , br [] []
                , text board.status
                ]
            ]
