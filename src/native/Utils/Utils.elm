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


flip_tuple : ( a, b ) -> ( b, a )
flip_tuple t =
    let
        t1 =
            Tuple.first t

        t2 =
            Tuple.second t
    in
        ( t2, t1 )


if_else : Bool -> a -> a -> a
if_else c a b =
    if c then
        a
    else
        b
