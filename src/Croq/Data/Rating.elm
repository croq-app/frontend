module Croq.Data.Rating exposing (Rating, decoder, encoder, max, min, rating, render, renderStars, tags, view)

import Html exposing (Html)
import Json.Decode as D
import Json.Encode as E


type Rating
    = Rating Int


minRating : number
minRating =
    0


maxRating : number
maxRating =
    2


{-| Rating in the range 1 to 5
-}
rating : Int -> Rating
rating n =
    if n < minRating then
        Rating minRating

    else if n > maxRating then
        Rating maxRating

    else
        Rating n


{-| Minimum rating
-}
min : Rating
min =
    rating minRating


{-| Maximum rating
-}
max : Rating
max =
    rating maxRating


tags : Rating -> List String
tags r =
    if r == max then
        [ "classic" ]

    else
        []


render : String -> String -> Rating -> String
render symb empty (Rating n) =
    String.repeat n symb ++ String.repeat n empty


renderStars : Rating -> String
renderStars =
    render "★" "☆"


view : (List (Html msg) -> Html msg) -> Html msg -> Html msg -> Rating -> Html msg
view wrapper elem empty (Rating n) =
    wrapper (List.repeat n elem ++ List.repeat n empty)


encoder : Rating -> E.Value
encoder (Rating n) =
    E.int n


decoder : D.Decoder Rating
decoder =
    D.map Rating D.int
