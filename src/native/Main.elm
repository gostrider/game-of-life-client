module Main exposing (..)

import Html exposing (Html, program)
import WebSocket as WS
import Models.Board as Board exposing (Board, BoardAction)
import View.Board as Board


main : Program Never Application Action
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Application =
    { board : Board
    }


type Action
    = UpdateBoard BoardAction


init : ( Application, Cmd Action )
init =
    Application Board.init ! []


update : Action -> Application -> ( Application, Cmd Action )
update action app =
    case action of
        UpdateBoard boardAction ->
            let
                ( board_, boardEff ) =
                    Board.update boardAction app.board
            in
                ( { app | board = board_ }, Cmd.map UpdateBoard boardEff )


view : Application -> Html Action
view app =
    Html.map UpdateBoard (Board.view app.board)


subscriptions : Application -> Sub Action
subscriptions app =
    let
        subscribeBoard =
            WS.listen "ws://localhost:8001/ws/test" Board.Incoming
    in
        Sub.batch
            [ Sub.map UpdateBoard subscribeBoard
            ]
