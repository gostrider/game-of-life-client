module Utils.Utils exposing (..)


flatten : List (List a) -> List a
flatten = List.concatMap identity


flip : a -> a -> a -> a
flip a b s =
    if s == a then
        b
    else
        a