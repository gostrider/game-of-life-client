module Models.Board exposing (..)

import WebSocket as WS
import Json.Encode as En
import Tuple as TP
import List as L


type alias Board =
    { width : Int
    , height : Int
    , input_seq : List ( Int, Int )
    , status : String
    }


type BoardAction
    = Send
    | ResetInput
    | Input Int Int
    | Incoming String


init : Board
init =
    Board 10 10 [] ""


update : BoardAction -> Board -> ( Board, Cmd BoardAction )
update action board =
    case action of
        Input x_ y_ ->
            let
                input_pair =
                    ( x_, y_ )
            in
                Debug.log (toString board.input_seq)
                    ( { board | input_seq = (input_pair :: board.input_seq) }, Cmd.none )

        Incoming message_ ->
            ( { board | status = message_ }, Cmd.none )

        Send ->
            let
                payload =
                    En.encode 0 (change board.input_seq)
            in
                ( board, ws_send payload )

        ResetInput ->
            let
                payload =
                    En.encode 0 (query "reset")
            in
                Debug.log (toString board.input_seq)
                    ( { board | input_seq = [] }, ws_send payload )


query : String -> En.Value
query action =
    En.object [ ( "action", En.string action ) ]


change : List ( Int, Int ) -> En.Value
change cells =
    let
        encoder =
            tuple2 En.int En.int
    in
        En.object
            [ ( "action", En.string "change" )
            , ( "cell", En.list (L.map encoder cells) )
            ]


tuple2 : (a -> En.Value) -> (b -> En.Value) -> ( a, b ) -> En.Value
tuple2 enc1 enc2 ( val1, val2 ) =
    En.list [ enc1 val1, enc2 val2 ]


ws_send : String -> Cmd BoardAction
ws_send =
    WS.send "ws://localhost:8001/ws/test"
