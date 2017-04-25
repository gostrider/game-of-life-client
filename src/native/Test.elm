module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random as R


main : Program Never Color Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Color =
    { display : String
    , backup : String
    }


type Msg
    = Roll
    | RandColor (List Int)


init : ( Color, Cmd Msg )
init =
    ( Color "rgb(0,0,0)" "rgb(0,0,0)", run )


update : Msg -> Color -> ( Color, Cmd Msg )
update msg color =
    case msg of
        RandColor color_ ->
            let
                by r acc =
                    toString r ++ "," ++ acc

                str_color =
                    color_
                        |> List.foldr by ""
                        |> (++) "rgb("
                        |> String.dropRight 1
                        |> flip (++) ")"
            in
                ( { color | backup = str_color }, Cmd.none )

        Roll ->
            ( { color | display = color.backup, backup = color.display }, Cmd.none )


view : Color -> Html Msg
view color =
    div [ style [ ( "color", color.display ) ] ]
        [ text (toString color)
        , br [] []
        , button [ onClick Roll ] [ text "flip" ]
        ]


subscriptions : Color -> Sub Msg
subscriptions color =
    Sub.none


run : Cmd Msg
run =
    R.generate RandColor
        (R.list 3
            (R.int 0 255)
        )
