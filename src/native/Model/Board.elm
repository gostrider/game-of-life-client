module Model.Board exposing (..)

import Json.Encode exposing (encode)
import WebSocket exposing (send)
import Matrix as M
import Random as R
import Time exposing (Time)
import Model.Cell as C exposing (Cell, CellAction)
import Utils.Request as Req
import Utils.Utils exposing (if_else)


type alias CellMatrix =
    M.Matrix Cell


type alias Cells =
    List Cell


type alias Board =
    { scale : Int
    , status : String
    , gen_color : String
    , pending : Cells
    , cells : CellMatrix
    }


type BoardAction
    = Reset
    | Send
    | Incoming String
    | UpdateCell CellAction
    | CreateColor (List Int)
    | UpdateScale String



-- | TimedSend Time


init : ( Board, Cmd BoardAction )
init =
    let
        scale =
            0

        cells =
            M.square 1 (C.init "")
    in
        ( Board scale "" "" [] cells, gen_color )


board_update : BoardAction -> Board -> ( Board, Cmd BoardAction )
board_update action board =
    case action of
        CreateColor color_ ->
            let
                str_color =
                    to_rgb color_

                cells_ =
                    M.square board.scale (C.init str_color)
            in
                ( { board | cells = cells_, gen_color = str_color, status = str_color }, Cmd.none )

        Reset ->
            let
                payload =
                    Req.encode_zero (Req.query "reset" board.gen_color)

                ( board_init, board_effect ) =
                    init
            in
                board_init
                    ! [ ws_send payload
                      , board_effect
                      ]

        UpdateScale scale_ ->
            let
                scale_int =
                    Result.withDefault 0 (String.toInt scale_)

                cells_ =
                    M.square scale_int (C.init board.gen_color)
            in
                ( { board | cells = cells_, scale = scale_int }, Cmd.none )

        Send ->
            let
                payload =
                    Req.encode_zero (Req.activity "change" board.gen_color board.pending)
            in
                ( board, ws_send payload )

        -- TimedSend _ ->
        --     let
        --         payload =
        --             Req.encode_zero (Req.activity "change" board.gen_color board.pending)
        --     in
        --         ( board, ws_send payload )
        Incoming message_ ->
            let
                decode_message =
                    message_
                        |> Req.decode_result
                        |> Result.withDefault []

                event =
                    message_
                        |> Req.decode_action
                        |> Result.withDefault ""

                cells_ =
                    render_result board decode_message
            in
                case event of
                    "result" ->
                        ( { board | cells = cells_, pending = decode_message }, Cmd.none )

                    "activity" ->
                        let
                            pending_ =
                                remove_repeated decode_message
                        in
                            ( { board | cells = cells_, pending = pending_ }, Cmd.none )

                    "reset" ->
                        let
                            ( board_init, board_effect ) =
                                init
                        in
                            board_init ! [ board_effect ]

                    _ ->
                        ( board, Cmd.none )

        UpdateCell (C.Update x y) ->
            let
                curr_pos =
                    ( x, y )

                cell_ =
                    curr_pos
                        |> flip M.get board.cells
                        |> Maybe.withDefault (C.init board.gen_color curr_pos)
                        |> C.cell_update (C.Update x y)

                cells_ =
                    update_cell curr_pos board.cells cell_

                pending_ =
                    upsert board.pending cell_

                payload =
                    Req.encode_zero (Req.activity "activity" board.gen_color pending_)
            in
                ( { board | cells = cells_, pending = pending_ }, ws_send payload )


ws_send : String -> Cmd BoardAction
ws_send =
    send "ws://localhost:8001/ws/game"



-- Board logic


to_rgb : List Int -> String
to_rgb =
    List.foldr (\color acc -> toString color ++ "," ++ acc) ""
        >> (++) "rgb("
        >> String.dropRight 1
        >> flip (++) ")"


gen_color : Cmd BoardAction
gen_color =
    R.generate CreateColor (R.list 3 (R.int 0 255))


remove_repeated : Cells -> Cells
remove_repeated =
    remove_repeated_ []


remove_repeated_ : Cells -> Cells -> Cells
remove_repeated_ acc cells =
    case cells of
        [] ->
            acc

        c :: cs ->
            if any_cell cs c then
                remove_repeated_ acc cs
            else
                remove_repeated_ (c :: acc) cs


render_result : Board -> Cells -> CellMatrix
render_result board cells =
    replace_cell (M.square board.scale (C.init board.gen_color)) cells


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


upsert : Cells -> Cell -> Cells
upsert x1 x2 =
    if_else
        (any_cell x1 x2)
        (remove_cell x1 x2)
        (append_cell x1 x2)
