module Model.Cell exposing (..)

import Matrix as M
import Utils.Utils as U


type alias Cell =
    { position : M.Location
    , alive : String
    , color : String
    }


type CellAction
    = Update Int Int


init : M.Location -> Cell
init pos =
    Cell pos "X" "white"


cell_update : CellAction -> Cell -> Cell
cell_update action cell =
    case action of
        Update x_ y_ ->
            let
                f_alive =
                    U.flip "X" "O" cell.alive

                f_color =
                    U.flip "white" "red" cell.color
            in
                { cell | position = M.loc x_ y_, alive = f_alive, color = f_color }
