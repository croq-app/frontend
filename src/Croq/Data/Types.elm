module Croq.Data.Types exposing (..)

import Html exposing (Attribute, Html)


type alias Id =
    String


type alias Name =
    String


type alias Person =
    String


type alias Text =
    String


type alias Beta =
    String


type alias Grade =
    String


type alias Year =
    Int


type alias Meter =
    Int


type alias Video =
    String


type alias Slug =
    String


type alias Url =
    String


type alias RefPath =
    List String


type alias Country =
    String


type alias LoadingError =
    String


type alias HtmlElem msg =
    List (Attribute msg) -> List (Html msg) -> Html msg


type alias Image =
    String


{-| Conditionally return ok or bad depending if x in present in xs.
-}
selectElem : a -> List a -> b -> b -> b
selectElem x xs ok bad =
    if List.member x xs then
        ok

    else
        bad


{-| Return a unique list making O(n^2) comparisons.

Avoid this function for long lists.

-}
uniqueSqr : List a -> List a
uniqueSqr elems =
    case elems of
        [] ->
            []

        x :: xs ->
            x :: List.filter ((/=) x) xs
