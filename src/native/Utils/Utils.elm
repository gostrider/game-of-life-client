module Utils.Utils exposing (..)


flatten : List (List a) -> List a
flatten =
    List.concatMap identity


flip : a -> a -> a -> a
flip a b s =
    if s == a then
        b
    else
        a


if_else : Bool -> a -> a -> a
if_else c a b =
    if c then
        a
    else
        b
