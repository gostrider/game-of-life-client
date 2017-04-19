module Model.Board
    exposing
        ( Board
        , BoardAction(..)
        , init
        , board_update
        )

import Json.Encode as En
import Json.Decode as De
import WebSocket exposing (send)
import Tuple exposing (first, second)
import Model.Cell as C exposing (Cell, CellAction)
import Utils.Utils as U


type alias Board =
    { width : Int
    , height : Int
    , cell : Cell
    , cells : List (List Cell)
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
    let
        width =
            5

        height =
            5

        cells =
            C.init_cells width height
    in
        Board width height C.init cells [] ""


board_update : BoardAction -> Board -> ( Board, Cmd BoardAction )
board_update action board =
    case action of
        Incoming message_ ->
            let
                decode_message =
                    decode_cells message_

                pending_ : List Cell
                pending_ =
                    Result.withDefault [] decode_message

                cells_ =
                    replace_multiple board.cells pending_ |> reorder
            in
                ( { board | status = message_, pending = pending_, cells = cells_ }
                , Cmd.none
                )

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

                cells_ =
                    C.init_cells board.width board.height
            in
                ( { board | cells = cells_, pending = [] }, ws_send payload )

        UpdateCell cellAction ->
            case cellAction of
                C.Update x_ y_ ->
                    let
                        current =
                            traverse_cells x_ y_ board.cells board.cell

                        cell_ =
                            C.cell_update cellAction current

                        cells_ =
                            cells_update board.cells cell_

                        pending_ =
                            pend_cell board.pending cell_
                    in
                        ( { board | cell = current, cells = cells_, pending = pending_ }, Cmd.none )


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
            , ( "cell", En.list (List.map encoder cells) )
            ]


tuple2 : (a -> En.Value) -> (b -> En.Value) -> ( a, b ) -> En.Value
tuple2 enc1 enc2 ( val1, val2 ) =
    En.list [ enc1 val1, enc2 val2 ]


decode_cells =
    De.decodeString (De.field "result" decode_cell)


decode_cell =
    De.list <|
        De.map4 Cell
            (De.field "x" De.int)
            (De.field "y" De.int)
            (De.field "alive" De.string)
            (De.field "color" De.string)


ws_send : String -> Cmd BoardAction
ws_send =
    send "ws://localhost:8001/ws/test"


cells_update =
    cells_update_ []


cells_update_ acc cells cell =
    case cells of
        [] ->
            List.reverse acc

        cs :: css ->
            let
                row =
                    replace_cell cs cell
            in
                cells_update_ (row :: acc) css cell


replace_cell cells cell =
    if member_of cell cells match_pos then
        cell :: find_distinct_cell cell cells |> List.sortBy C.x_
    else
        cells

reorder cells =
    let
        each_y c = List.map C.y_ c
    in
        List.sortBy each_y cells


replace_multiple cells cells_ =
    case cells_ of
        [] ->
            cells

        c_ :: cs_ ->
            let
                flatten =
                    List.concatMap identity

                match_row = List.partition (find_row c_) cells

                member =
                    Tuple.first match_row |> flip pend_cell c_ << flatten

                p_member =
                    List.partition (\x -> C.alive_ x == "O") member

                new_cells =
                    c_ :: (Tuple.first p_member |> change_cell []) ++ (Tuple.second p_member) |> List.sortBy C.x_

                elements =
                    new_cells :: (Tuple.second match_row)
            in
                replace_multiple elements cs_


change_cell acc cells =
    case cells of
        [] ->
            acc

        c :: cs ->
            let
                flip_alive =
                    c |> U.flip "X" "O" << C.alive_

                flip_color =
                    c |> U.flip "grey" "red" << C.color_

                element =
                    c |> C.alive__ flip_alive << C.color__ flip_color
            in
                change_cell (element :: acc) cs


find_row cell cells =
    case cells of
        [] ->
            False

        c :: cs ->
            if C.y_ c == C.y_ cell || C.x_ c == C.x_ cell then
                True
            else
                False


match_pos : Cell -> Cell -> Bool
match_pos c1 c2 =
    C.pos_ c1 == C.pos_ c2


member_of : Cell -> List Cell -> (Cell -> Cell -> Bool) -> Bool
member_of cell cells f =
    case cells of
        [] ->
            False

        c :: cs ->
            if f c cell then
                True
            else
                member_of cell cs f


find_distinct_cell c cs =
    List.filter (not << (match_pos c)) cs


pend_cell cells cell =
    if member_of cell cells match_pos then
        find_distinct_cell cell cells
    else
        cell :: cells


traverse_cells x y cells default =
    case cells of
        [] ->
            default

        cs :: css ->
            let
                find_pos x y c =
                    C.x_ c == x && C.y_ c == y
            in
                case List.filter (find_pos x y) cs of
                    [] ->
                        traverse_cells x y css default

                    [ c ] ->
                        c

                    _ ->
                        default
