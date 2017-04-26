module View.Board exposing (..)

import Html exposing (Html, div, button, br, h1, input, table, text)
import Html.Attributes exposing (align, type_, style)
import Html.Events exposing (onClick, onInput)


-- Application modules

import Model.Board exposing (Board, BoardAction(..))
import View.Cell exposing (draw_row)
import View.UserConfig exposing (config_view)


board_view : Board -> Html BoardAction
board_view board =
    if board.scale >= 3 || board.user_config.pattern /= "" then
        display_grid board
    else
        display_init board


display_grid : Board -> Html BoardAction
display_grid board =
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
                , div [ style [ ( "background-color", board.gen_color ) ] ] [ text board.status ]
                ]
            ]


display_init : Board -> Html BoardAction
display_init board =
    config_view board.user_config |> Html.map UpdateConfig
