module Model.Cell exposing (..)


type alias Cell =
    { x : Int
    , y : Int
    , alive : String
    , color : String
    }


type CellAction
    = Update Int Int


init : Cell
init =
    Cell 0 0 "X" "grey"


pos_ : Cell -> ( Int, Int )
pos_ { x, y } =
    ( x, y )


alive_ : Cell -> String
alive_ { alive } =
    alive


color_ : Cell -> String
color_ { color } =
    color


pos__ : Int -> Int -> Cell -> Cell
pos__ x y { alive, color } =
    Cell x y alive color


alive__ : String -> Cell -> Cell
alive__ alive { x, y, color } =
    Cell x y alive color


color__ : String -> Cell -> Cell
color__ color { x, y, alive } =
    Cell x y alive color


cell_update : CellAction -> Cell -> Cell
cell_update action cell =
    case action of
        Update x_ y_ ->
            { cell | x = x_, y = y_ }
