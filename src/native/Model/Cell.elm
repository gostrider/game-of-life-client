module Model.Cell exposing (..)

import Matrix as M
import Utils.Utils as U


type alias Cell =
    { position : M.Location
    , alive : ( String, String )
    , color : ( String, String )
    }


type CellAction
    = Update Int Int


init : String -> M.Location -> Cell
init color pos =
    let
        default_alive =
            ( "X", "O" )

        default_color =
            ( "rgb(255,255,255)", color )
    in
        Cell pos default_alive default_color


cell_update : CellAction -> Cell -> Cell
cell_update action cell =
    case action of
        Update x_ y_ ->
            { cell
                | position = ( x_, y_ )
                , alive = U.flip_tuple cell.alive
                , color = U.flip_tuple cell.color
            }
