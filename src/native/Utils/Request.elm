module Utils.Request exposing (..)

import Json.Decode as De
import Json.Encode as En
import Model.Cell exposing (Cell)
import Matrix as M
import Array as A


-- Encode operations


query : String -> En.Value
query action =
    En.object [ ( "action", En.string action ) ]


change : List Cell -> En.Value
change cells =
    let
        encode_cells =
            En.list << List.map encode_position
    in
        En.object
            [ ( "action", En.string "change" )
            , ( "cell", encode_cells cells )
            ]


encode_position : Cell -> En.Value
encode_position cell =
    En.object
        [ ( "x", cell.position |> M.row >> En.int )
        , ( "y", cell.position |> M.col >> En.int )
        ]



-- Decode operations


decode_cells : String -> Result String (List Cell)
decode_cells =
    De.decodeString
        (De.field "result" decode_cell)


decode_cell : De.Decoder (List Cell)
decode_cell =
    De.list <|
        De.map3 Cell
            decode_position
            (De.field "alive" De.string)
            (De.field "color" De.string)


decode_position : De.Decoder M.Location
decode_position =
    De.map2 M.loc
        (De.field "x" De.int)
        (De.field "y" De.int)
