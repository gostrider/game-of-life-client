module Utils.Request exposing (..)

import Json.Decode as De
import Json.Encode as En
import Matrix as M


-- Application modules

import Model.Cell exposing (Cell)


-- Encode operations


encode_zero : En.Value -> String
encode_zero =
    En.encode 0


query : String -> String -> En.Value
query action color =
    En.object
        [ ( "color", En.string color )
        , ( "action", En.string action )
        ]


activity : String -> String -> List Cell -> En.Value
activity action color cells =
    let
        encode_cells =
            En.list << List.map encode_cell
    in
        En.object
            [ ( "color", En.string color )
            , ( "action", En.string action )
            , ( "cells", encode_cells cells )
            ]


encode_cell : Cell -> En.Value
encode_cell cell =
    En.object
        [ ( "x", cell.position |> M.row >> En.int )
        , ( "y", cell.position |> M.col >> En.int )
        , ( "alive", tuple_to_list cell.alive )
        , ( "color", tuple_to_list cell.color )
        ]


tuple_to_list : ( String, String ) -> En.Value
tuple_to_list ( a, b ) =
    En.list [ En.string a, En.string b ]



-- Decode operations


decode_result : String -> Result String (List Cell)
decode_result =
    De.decodeString (De.field "cells" decode_cell)


decode_action : String -> Result String String
decode_action =
    De.decodeString (De.field "action" De.string)


decode_color : String -> Result String String
decode_color =
    De.decodeString (De.field "color" De.string)


decode_cell : De.Decoder (List Cell)
decode_cell =
    De.list
        (De.map3 Cell
            decode_position
            (from_list "alive")
            (from_list "color")
        )


decode_position : De.Decoder M.Location
decode_position =
    De.map2 M.loc
        (De.field "x" De.int)
        (De.field "y" De.int)


from_list : String -> De.Decoder ( String, String )
from_list name =
    De.map2 (,)
        (De.field name (De.index 0 De.string))
        (De.field name (De.index 1 De.string))
