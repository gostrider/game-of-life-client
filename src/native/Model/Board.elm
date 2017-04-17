module Model.Board
    exposing
        ( Board
        , BoardAction(..)
        , init
        , board_update
        )

import Json.Encode as En
import WebSocket exposing (send)
import List exposing (filter, map)
import Tuple exposing (first, second)
import Model.Cell as C exposing (Cell, CellAction)


type alias Board =
    { width : Int
    , height : Int
    , cell : Cell
    , pending : List Cell
    , status : String
    }


type BoardAction
    = Send
    | ResetInput
    | Incoming String
    | UpdateCell CellAction


init : Board
init =
    Board 10 10 C.init [] ""


board_update : BoardAction -> Board -> ( Board, Cmd BoardAction )
board_update action board =
    case action of
        Incoming message_ ->
            ( { board | status = message_ }, Cmd.none )

        Send ->
            let
                payload =
                    En.encode 0 (change board.pending)
            in
                ( board, ws_send payload )

        ResetInput ->
            let
                payload =
                    En.encode 0 (query "reset")
            in
                ( { board | pending = [] }, ws_send payload )

        UpdateCell cellAction ->
            let
                cell_ =
                    C.cell_update cellAction board.cell

                new_pending =
                    toggle_cell board.pending cell_
            in
                ( { board | cell = cell_, pending = new_pending }, Cmd.none )


query : String -> En.Value
query action =
    En.object [ ( "action", En.string action ) ]


change : List Cell -> En.Value
change cells =
    let
        cell_position =
            C.pos_

        encoder cell =
            En.object
                [ ( "x", cell |> cell_position >> first >> En.int )
                , ( "y", cell |> cell_position >> second >> En.int )
                ]
    in
        En.object
            [ ( "action", En.string "change" )
            , ( "cell", En.list (map encoder cells) )
            ]


tuple2 : (a -> En.Value) -> (b -> En.Value) -> ( a, b ) -> En.Value
tuple2 enc1 enc2 ( val1, val2 ) =
    En.list [ enc1 val1, enc2 val2 ]


ws_send : String -> Cmd BoardAction
ws_send =
    send "ws://localhost:8001/ws/test"


match_pos : Cell -> Cell -> Bool
match_pos c1 c2 =
    C.pos_ c1 == C.pos_ c2


toggle_cell : List Cell -> Cell -> List Cell
toggle_cell cells cell =
    if member_of cell cells match_pos then
        filter (not << (match_pos cell)) cells
    else
        cell :: cells


member_of : Cell -> List Cell -> (Cell -> Cell -> Bool) -> Bool
member_of cell cells f =
    case cells of
        [] ->
            False

        c :: cs ->
            let
                predicate =
                    f c cell
            in
                if predicate then
                    predicate
                else
                    member_of cell cs f
