module Utils.Request exposing (..)

import Json.Decode as De
import Json.Encode as En
import Model.Cell exposing (Cell)
import Matrix as M


-- Encode operations


query : String -> En.Value
query action =
    En.object [ ( "action", En.string action ) ]


activity : String -> List Cell -> En.Value
activity action cells =
    let
        encode_cells =
            En.list << List.map encode_position
    in
        En.object
            [ ( "action", En.string action )
            , ( "cells", encode_cells cells )
            ]


encode_position : Cell -> En.Value
encode_position cell =
    En.object
        [ ( "x", cell.position |> M.row >> En.int )
        , ( "y", cell.position |> M.col >> En.int )
        , ( "alive", En.string cell.alive )
        , ( "color", En.string cell.color )
        ]



-- Decode operations


decode_result : String -> Result String (List Cell)
decode_result =
    De.decodeString
        (De.field "action" De.string |> De.andThen decode_action)


decode_action : String -> De.Decoder (List Cell)
decode_action action =
    case action of
        "activity" ->
            De.field "cells" decode_cell

        "result" ->
            De.field "cells" decode_cell

        "query" ->
            De.field "cells" decode_cell

        _ ->
            De.fail "unknown_event"


decode_cell : De.Decoder (List Cell)
decode_cell =
    De.list
        (De.map3 Cell
            decode_position
            (De.field "alive" De.string)
            (De.field "color" De.string)
        )


decode_position : De.Decoder M.Location
decode_position =
    De.map2 M.loc
        (De.field "x" De.int)
        (De.field "y" De.int)
