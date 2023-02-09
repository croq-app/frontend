module Croq.Data.Loading exposing (Loading(..), LoadingHttp, andThen, fromHttpError, fromMaybe, fromResult, map, map2, map4, map5, mapError, showHttpError, toHttpResult, toMaybe, toResult, unwrap, view, withDefault, toHttp)

{-| Loading is similar to `Result err a`, but it has tree states:

    Success a: Represents a successful computation
    Error err: Represents an error
    Pending: Intermediate pending loading some data.

-}

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Http


type Loading err a
    = Pending
    | Success a
    | Error err


type alias LoadingHttp a =
    Loading Http.Error a


fromResult : Result err a -> Loading err a
fromResult result =
    case result of
        Ok data ->
            Success data

        Err error ->
            Error error


fromMaybe : err -> Maybe a -> Loading err a
fromMaybe err data =
    case data of
        Just x ->
            Success x

        Nothing ->
            Error err


toMaybe : Loading err a -> Maybe a
toMaybe =
    unwrap Nothing Just


toResult : err -> Loading err a -> Result err a
toResult error data =
    case data of
        Success x ->
            Ok x

        Error e ->
            Err e

        Pending ->
            Err error


ap : Loading err (a -> value) -> Loading err a -> Loading err value
ap f data =
    case ( f, data ) of
        ( Success g, Success x ) ->
            Success (g x)

        ( Error e, _ ) ->
            Error e

        ( _, Error e ) ->
            Error e

        _ ->
            Pending


map : (a -> value) -> Loading err a -> Loading err value
map f =
    andThen (f >> Success)


map2 : (a -> b -> value) -> Loading err a -> Loading err b -> Loading err value
map2 f =
    andThen (f >> Success) >> ap


map3 : (a -> b -> c -> value) -> Loading err a -> Loading err b -> Loading err c -> Loading err value
map3 f a b =
    ap (map2 f a b)


map4 : (a -> b -> c -> d -> value) -> Loading err a -> Loading err b -> Loading err c -> Loading err d -> Loading err value
map4 f a b c =
    ap (map3 f a b c)


map5 : (a -> b -> c -> d -> e -> value) -> Loading err a -> Loading err b -> Loading err c -> Loading err d -> Loading err e -> Loading err value
map5 f a b c d =
    ap (map4 f a b c d)


mapError : (err -> a) -> Loading err b -> Loading a b
mapError f data =
    case data of
        Error e ->
            Error (f e)

        Success x ->
            Success x

        Pending ->
            Pending


andThen : (a -> Loading err b) -> Loading err a -> Loading err b
andThen f data =
    case data of
        Success x ->
            f x

        Pending ->
            Pending

        Error x ->
            Error x


unwrap : a -> (b -> a) -> Loading err b -> a
unwrap default f data =
    case data of
        Success x ->
            f x

        _ ->
            default


withDefault : a -> Loading err a -> a
withDefault default =
    unwrap default identity


view : LoadingHttp a -> (a -> Html msg) -> Html msg
view data render =
    case data of
        Success content ->
            render content

        Error error ->
            div [ class "card m-4 p-4 bg-focus" ] [ text (showHttpError error) ]

        Pending ->
            div [ class "card m-4 p-4 bg-focus" ] [ text "Carregando..." ]


fromHttpError : String -> LoadingHttp a
fromHttpError msg =
    Error (Http.BadBody msg)


toHttpResult : LoadingHttp a -> Result Http.Error a
toHttpResult =
    toResult Http.NetworkError


toHttp : Loading String a -> LoadingHttp a
toHttp =
    mapError Http.BadBody


showHttpError : Http.Error -> String
showHttpError error =
    case error of
        Http.BadUrl str ->
            "bad url: " ++ str

        Http.Timeout ->
            "operation timedout"

        Http.NetworkError ->
            "network error"

        Http.BadStatus code ->
            "bad status code: " ++ String.fromInt code

        Http.BadBody body ->
            "invalid response: " ++ body
