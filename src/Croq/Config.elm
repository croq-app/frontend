module Croq.Config exposing (..)

import Browser.Navigation as Nav exposing (Key)
import Dict exposing (Dict)
import Http exposing (Error(..))


type alias Model =
    { navKey : Key, api : String, static : String, translations : Dict String String }


type Msg
    = NoOp


init : String -> Key -> Model
init hostname key =
    case hostname of
        "croq-app.github.io" ->
            { navKey = key
            , api = "/frontend/api/"
            , static = "/frontend/static/"
            , translations = Dict.empty
            }

        _ ->
            { navKey = key
            , api = "/api/"
            , static = "/static/"
            , translations = Dict.empty
            }


tr : Model -> String -> String
tr m st =
    Dict.get st m.translations
        |> Maybe.withDefault st


update : Msg -> Model -> ( Model, Cmd Msg )
update _ m =
    ( m, Cmd.none )


pushUrl : String -> Model -> Cmd msg
pushUrl url m =
    Nav.pushUrl m.navKey url


pushErrorUrl : Error -> Model -> Cmd msg
pushErrorUrl e m =
    Nav.pushUrl m.navKey ("/error?msg=" ++ errorMsg e)


errorMsg : Error -> String
errorMsg e =
    case e of
        BadUrl url ->
            "bad url: " ++ url

        Timeout ->
            "operation timed out"

        NetworkError ->
            "network error"

        BadStatus i ->
            "bad status: " ++ String.fromInt i

        BadBody st ->
            "bad body: " ++ st
