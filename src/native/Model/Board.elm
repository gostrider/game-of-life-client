module Model.Board
    exposing
        ( Board
        , BoardAction(..)
        , init
        , board_update
        )

import Json.Encode exposing (encode)
import WebSocket exposing (send)
import Model.Cell as C exposing (Cell, CellAction)
import Utils.Utils as U
import Utils.Request as Request


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
                    Request.decode_cells message_

                pending_ : List Cell
                pending_ =
                    Result.withDefault [] decode_message

                cells_ =
                    replace_multiple_ board.cells pending_ |> reorder
            in
                ( { board | status = message_, pending = pending_, cells = cells_ }
                , Cmd.none
                )

        Send ->
            let
                payload =
                    encode 0 (Request.change board.pending)
            in
                ( board, ws_send payload )

        ResetInput ->
            let
                payload =
                    encode 0 (Request.query "reset")

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
                            append_cell board.pending cell_
                    in
                        ( { board | cell = current, cells = cells_, pending = pending_ }, Cmd.none )


ws_send : String -> Cmd BoardAction
ws_send =
    send "ws://localhost:8001/ws/test"



-- Cell operations


cells_update : List (List Cell) -> Cell -> List (List Cell)
cells_update cells cell =
    let
        by : List Cell -> List (List Cell) -> List (List Cell)
        by cell_ =
            (::) (replace_cell cell_ cell |> List.sortBy C.x_)
    in
        List.foldl by [] cells |> List.reverse


replace_multiple_ : List (List Cell) -> List Cell -> List (List Cell)
replace_multiple_ origin_cells mutate_cells =
    case mutate_cells of
        [] ->
            origin_cells

        m_cell :: m_cells ->
            let
                mutate_row : Cell -> ( List (List Cell), List (List Cell) )
                mutate_row =
                    get_x_y_members origin_cells

                mutate_members : ( List (List Cell), List (List Cell) ) -> List Cell
                mutate_members =
                    flip append_cell m_cell << U.flatten << Tuple.first

                alive_members : List Cell -> ( List Cell, List Cell )
                alive_members =
                    List.partition ((==) "O" << C.alive_)

                new_row : ( List Cell, List Cell ) -> List Cell
                new_row =
                    flip join_cells m_cell

                run : Cell -> List Cell
                run =
                    new_row << alive_members << mutate_members << mutate_row

                elements : List (List Cell)
                elements =
                    (run m_cell) :: (m_cell |> Tuple.second << mutate_row)
            in
                replace_multiple_ elements m_cells


mutate_cells : List Cell -> List Cell
mutate_cells cells =
    let
        f_alive : Cell -> Cell
        f_alive c =
            C.alive__ (U.flip "X" "O" (C.alive_ c)) c

        f_color : Cell -> Cell
        f_color c =
            C.color__ (U.flip "grey" "red" (C.color_ c)) c

        element : Cell -> List Cell -> List Cell
        element cell =
            (::) (cell |> f_color << f_alive)
    in
        List.foldl element [] cells


traverse_cells : Int -> Int -> List (List Cell) -> Cell -> Cell
traverse_cells x y cells default =
    let
        by : List Cell -> List Cell -> List Cell
        by cs _ =
            List.filter (match_x_y x y) cs

        found : Maybe Cell
        found =
            List.foldl by [] cells |> List.head
    in
        Maybe.withDefault default found



-- Boolean combinator


match_x_y : Int -> Int -> Cell -> Bool
match_x_y x y cell =
    (C.x_ cell == x) && (C.y_ cell == y)


match_pos : Cell -> Cell -> Bool
match_pos cell1 cell2 =
    C.pos_ cell1 == C.pos_ cell2


match_x_or_y : Cell -> Cell -> Bool
match_x_or_y cell1 cell2 =
    (C.y_ cell1 == C.y_ cell2) || (C.x_ cell1 == C.x_ cell2)



-- List combinator


fmapBool : (a -> Bool) -> Maybe a -> Bool
fmapBool f x =
    Maybe.withDefault False (Maybe.map f x)


any_x_y : Cell -> List Cell -> Bool
any_x_y cell =
    fmapBool (match_x_or_y cell) << List.head


member_of : (a -> Bool) -> List a -> Bool
member_of f cells =
    List.any f cells


reorder : List (List Cell) -> List (List Cell)
reorder cells =
    List.sortBy (List.map C.y_) cells


get_row_members : List Cell -> Cell -> List Cell
get_row_members cells cell =
    List.filter (not << (match_pos cell)) cells


get_x_y_members :
    List (List Cell)
    -> Cell
    -> ( List (List Cell), List (List Cell) )
get_x_y_members cells cell =
    List.partition (any_x_y cell) cells


join_cells : ( List Cell, List Cell ) -> Cell -> List Cell
join_cells mutate_and_remain cell =
    cell
        :: (mutate_and_remain |> mutate_cells << Tuple.first)
        ++ (mutate_and_remain |> Tuple.second)
        |> List.sortBy C.x_



-- Control flow


replace_cell : List Cell -> Cell -> List Cell
replace_cell cells cell =
    if member_of (match_pos cell) cells then
        cell :: get_row_members cells cell
    else
        cells


append_cell : List Cell -> Cell -> List Cell
append_cell cells cell =
    if member_of (match_pos cell) cells then
        get_row_members cells cell
    else
        cell :: cells
