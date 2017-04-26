module View.UserConfig exposing (..)

import Html exposing (Html, br, div, h2, hr, input, option, select, text)
import Html.Attributes exposing (align, placeholder, selected, type_)
import Html.Events exposing (onInput)


-- Application modules

import Model.UserConfig exposing (Config, ConfigUpdate(..))


config_view : Config -> Html ConfigUpdate
config_view config =
    div [ align "center" ]
        [ h2 [] [ text "Please enter game size" ]
        , input [ type_ "text", placeholder "Size of the board", onInput Size ] []
        , text " or "
        , select [ onInput Pattern ]
            [ option [ selected True ] [ text "Select a template to start" ]
            , option [] [ text "blinker_and_toad" ]
            , option [] [ text "pulsar" ]
            , option [] [ text "glider" ]
            , option [] [ text "unbounded_growth" ]
            ]
        , hr [] []
        , text "Instructions"
        , br [] []
        , br [] []
        , text "1. Insert the board size you prefer."
        , br [] []
        , text "2. Or select a template to start with."
        , br [] []
        , text "3. Place the alive cell on the grid."
        , br [] []
        , text "4. Press Start to start evolve."
        , br [] []
        , text "5. Press reset to return to current page."
        , br [] []
        , br [] []
        , text "The color represent your own color."
        , br [] []
        , text "Winner color will represent during the evolve process"
        , br [] []
        , br [] []
        , text "enjoy"
          --        , select [ onInput Count ]
          --            [ option [ selected True ] [ text "10" ]
          --            , option [] [ text "30" ]
          --            , option [] [ text "60" ]
          --            ]
          --        , text " second(s)"
        ]
