module View.Application exposing (app_view)

import Html as Html exposing (Html)
import Model.Application exposing (Application, Action(UpdateBoard))
import View.Board exposing (board_view)


app_view : Application -> Html Action
app_view app =
    Html.map UpdateBoard (board_view app.board)
