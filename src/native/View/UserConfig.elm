module View.UserConfig exposing (..)

import Html exposing (Html, div, h2, hr, input, option, select, text)
import Html.Attributes exposing (align, placeholder, selected, type_)
import Html.Events exposing (onInput)
import Model.UserConfig exposing (Config, ConfigUpdate(..))


config_view : Config -> Html ConfigUpdate
config_view config =
    div [ align "center" ]
        [ h2 [] [ text "Please enter game size and survival time" ]
        , input [ type_ "text", placeholder "Size of the board", onInput Size ] []
        , text " or "
        , select [ onInput Pattern ]
            [ option [ selected True ] [ text "Select a template to start" ]
            , option [] [ text "blinker_and_toad" ]
            , option [] [ text "pulsar" ]
            , option [] [ text "glider" ]
            ]
        , hr [] []
        , select [ onInput Count ]
            [ option [ selected True ] [ text "10" ]
            , option [] [ text "30" ]
            , option [] [ text "60" ]
            ]
        , text " second(s)"
        ]
