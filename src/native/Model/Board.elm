module Model.Board exposing (..)

import Json.Encode exposing (encode)
import WebSocket exposing (send)
import Matrix as M
import Model.Cell as C exposing (Cell, CellAction)
import Utils.Request as Request
import Utils.Utils exposing (if_else)


type alias CellMatrix =
    M.Matrix Cell


type alias Cells =
    List Cell


type alias Board =
    { scale : Int
    , cell : Cell
    , cells : CellMatrix
    , pending : Cells
    , status : String
    }


type BoardAction
    = Send
    | Reset
    | Incoming String
    | UpdateCell CellAction


init : ( Board, Cmd BoardAction )
init =
    let
        scale =
            5

        cell =
            C.init ( 0, 0 )

        cells =
            M.square scale C.init
    in
        ( Board scale cell cells [] "", Cmd.none )


board_update : BoardAction -> Board -> ( Board, Cmd BoardAction )
board_update action board =
    case action of
        UpdateCell (C.Update x y) ->
            let
                curr_pos =
                    M.loc x y

                cell_ =
                    curr_pos
                        |> flip M.get board.cells
                        |> Maybe.withDefault (C.init curr_pos)
                        |> C.cell_update (C.Update x y)

                cells_ =
                    update_cell curr_pos board.cells cell_

                pending_ =
                    tri_map board.pending cell_

                payload =
                    encode 0 (Request.activity "activity" pending_)
            in
                ( { board | cells = cells_, pending = pending_ }, ws_send payload )

        Incoming message_ ->
            let
                decode_message =
                    message_
                        |> Request.decode_result
                        |> Result.withDefault []

                cells_ =
                    render_result board.scale decode_message board.cell
            in
                ( { board | cells = cells_, pending = decode_message }, Cmd.none )

        Send ->
            let
                payload =
                    encode 0 (Request.activity "change" board.pending)
            in
                ( board, ws_send payload )

        Reset ->
            let
                payload =
                    encode 0 (Request.query "reset")

                ( board_init, _ ) =
                    init
            in
                ( board_init, ws_send payload )


ws_send : String -> Cmd BoardAction
ws_send =
    send "ws://localhost:8001/ws/test"



-- cell operations


render_result : Int -> Cells -> Cell -> CellMatrix
render_result scale cells cell =
    replace_cell (M.square scale C.init) cells


replace_cell : CellMatrix -> Cells -> CellMatrix
replace_cell base cells =
    case cells of
        [] ->
            base

        c :: cs ->
            replace_cell (M.set c.position c base) cs


update_cell : M.Location -> CellMatrix -> Cell -> CellMatrix
update_cell pos cells cell =
    M.update pos (always cell) cells



-- Boolean operations


match_position : Cell -> Cell -> Bool
match_position c1 c2 =
    c1.position == c2.position


any_cell : Cells -> Cell -> Bool
any_cell cells cell =
    List.any (match_position cell) cells



-- Control Flow


append_cell : Cells -> Cell -> Cells
append_cell cells cell =
    cell :: cells


remove_cell : Cells -> Cell -> Cells
remove_cell cells cell =
    List.filter (not << (match_position cell)) cells


tri_map : Cells -> Cell -> Cells
tri_map x1 x2 =
    if_else
        (any_cell x1 x2)
        (remove_cell x1 x2)
        (append_cell x1 x2)
