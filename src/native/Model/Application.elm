module Model.Application exposing (..)

import WebSocket exposing (listen)
import Time exposing (every, second, minute)


-- Application modules

import Model.Board as Board exposing (Board, BoardAction, board_update)


type alias Application =
    { board : Board
    }


type Action
    = UpdateBoard BoardAction


init : ( Application, Cmd Action )
init =
    let
        ( board_init, board_eff ) =
            Board.init
    in
        Application board_init ! [ Cmd.map UpdateBoard board_eff ]


update : Action -> Application -> ( Application, Cmd Action )
update action app =
    case action of
        UpdateBoard boardAction ->
            let
                ( board_, boardEff ) =
                    board_update boardAction app.board
            in
                ( { app | board = board_ }, Cmd.map UpdateBoard boardEff )


subscriptions : Application -> Sub Action
subscriptions app =
    let
        subscribeBoard =
            listen "ws://localhost:8001/ws/game" Board.Incoming

        timed_effect =
            every second Board.TimedSend
    in
        Sub.batch
            [ Sub.map UpdateBoard subscribeBoard
            , Sub.map UpdateBoard timed_effect
            ]
