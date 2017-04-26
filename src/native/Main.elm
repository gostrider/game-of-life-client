module Main exposing (main)

import Html exposing (program)


-- Application modules

import Model.Application as App exposing (Application, Action)
import View.Application exposing (app_view)


main : Program Never Application Action
main =
    program
        { init = App.init
        , view = app_view
        , update = App.update
        , subscriptions = App.subscriptions
        }
