module Model.UserConfig exposing (..)

import String exposing (toInt)


type alias Config =
    { size : Int
    , count : Int
    , pattern : String
    }


type ConfigUpdate
    = Size String
    | Count String
    | Pattern String


init : Config
init =
    Config 0 10 ""


config_update : ConfigUpdate -> Config -> Config
config_update action config =
    case action of
        Size s ->
            { config | size = defaultInt s }

        Count c ->
            { config | count = defaultInt c }

        Pattern p ->
            { config | size = 20, pattern = p }


defaultInt : String -> Int
defaultInt i =
    Result.withDefault 0 (toInt i)
