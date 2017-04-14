module View.Board exposing (..)

import Html exposing (Html, div, button, br, h1, input, table, td, text, tr)
import Html.Attributes exposing (align, style, type_)
import Html.Events exposing (onClick)
import Models.Board exposing (Board, BoardAction(..))


view : Board -> Html BoardAction
view board =
    div [ align "center" ]
        [ h1 [] [ text "title" ]
        , table [] (draw_board board.width board.height)
        , br [] []
        , div []
            [ button [ onClick ResetInput ] [ text "Reset" ]
            , button [ onClick Send ] [ text "Start" ]
            , br [] []
            , br [] []
            , text board.status
            ]
        ]


cell : Int -> Int -> String -> Html BoardAction
cell x y z =
    td [ style [ ( "background-color", "grey" ) ] ]
        [ input [ type_ "checkbox", onClick (Input x y) ] [] ]


draw_board : Int -> Int -> List (Html BoardAction)
draw_board =
    draw_row []


draw_row : List (Html BoardAction) -> Int -> Int -> List (Html BoardAction)
draw_row rows row column =
    let
        next_row =
            row - 1

        columns =
            draw_column row column []

        row_element =
            tr [] columns
    in
        case row of
            0 ->
                rows

            _ ->
                draw_row (row_element :: rows) next_row column


draw_column : Int -> Int -> List (Html BoardAction) -> List (Html BoardAction)
draw_column row column columns =
    let
        next_col =
            column - 1

        cell_element =
            cell column row "-X-"
    in
        case column of
            0 ->
                columns

            _ ->
                draw_column row next_col (cell_element :: columns)
