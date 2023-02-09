module Croq.Util exposing (filterNullableKey, iff, iterate, maybeCompare, maybeShow, repeats, maybeCompareLast)

import List.Extra as List


repeats : List comparable -> List ( comparable, Int )
repeats xs =
    let
        do ys acc =
            case ys of
                a :: rest ->
                    let
                        ( left, right ) =
                            List.span (\x -> x == a) rest
                    in
                    do right (( a, List.length left + 1 ) :: acc)

                [] ->
                    acc
    in
    List.reverse (do xs [])


iterate : (a -> a) -> a -> Int -> List a
iterate func x0 n =
    if n == 0 then
        [ x0 ]

    else
        x0 :: iterate func (func x0) (n - 1)


filterNullableKey : (a -> b) -> Maybe b -> List a -> List a
filterNullableKey key value xs =
    case value of
        Just x ->
            List.filter (\elem -> key elem == x) xs

        _ ->
            xs


maybeShow : (a -> String) -> Maybe a -> String
maybeShow show mx =
    case mx of
        Just x ->
            show x

        _ ->
            "∅"


maybeCompare : (a -> b -> Order) -> Maybe a -> Maybe b -> Order
maybeCompare f mx my =
    case ( mx, my ) of
        ( Just x, Just y ) ->
            f x y

        ( Nothing, Nothing ) ->
            EQ

        ( Nothing, _ ) ->
            LT

        ( _, Nothing ) ->
            GT


maybeCompareLast : (a -> b -> Order) -> Maybe a -> Maybe b -> Order
maybeCompareLast f mx my =
    case ( mx, my ) of
        ( Just x, Just y ) ->
            f x y

        ( Nothing, Nothing ) ->
            EQ

        ( Nothing, _ ) ->
            GT

        ( _, Nothing ) ->
            LT


iff : Bool -> c -> c -> c
iff a b c =
    if a then
        b

    else
        c
