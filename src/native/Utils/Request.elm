module Utils.Request exposing (..)

import Json.Decode as De
import Json.Encode as En
import Model.Cell exposing (Cell)

query : String -> En.Value
query action =
    En.object [ ( "action", En.string action ) ]


change : List Cell -> En.Value
change cells =
    let
        cell_position =
            Model.Cell.pos_

        encoder cell =
            En.object
                [ ( "x", cell |> cell_position >> Tuple.first >> En.int )
                , ( "y", cell |> cell_position >> Tuple.second >> En.int )
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