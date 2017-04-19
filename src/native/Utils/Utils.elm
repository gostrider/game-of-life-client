module Utils.Utils exposing (..)


flip : a -> a -> a -> a
flip a b s =
    if s == a then
        b
    else
        a