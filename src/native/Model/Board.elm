module Model.Board exposing (..)

import WebSocket as WS
import Json.Encode as En
import List as L
import Tuple as T
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


update : BoardAction -> Board -> ( Board, Cmd BoardAction )
update action board =
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
                ({ board | cell = cell_, pending = new_pending }, Cmd.none)


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
                [ ("x", cell |> cell_position >> T.first >> En.int)
                , ("y", cell |> cell_position >> T.second >> En.int)
                ]
    in
        En.object
            [ ( "action", En.string "change" )
            , ( "cell", En.list (L.map encoder cells) )
            ]


tuple2 : (a -> En.Value) -> (b -> En.Value) -> ( a, b ) -> En.Value
tuple2 enc1 enc2 ( val1, val2 ) =
    En.list [ enc1 val1, enc2 val2 ]


ws_send : String -> Cmd BoardAction
ws_send =
    WS.send "ws://localhost:8001/ws/test"


match_pos : Cell -> Cell -> Bool
match_pos c1 c2 =
    C.pos_ c1 == C.pos_ c2


toggle_cell : List Cell -> Cell -> List Cell
toggle_cell cells cell =
    if member_of cell cells match_pos then
        L.filter (not << (match_pos cell)) cells
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
