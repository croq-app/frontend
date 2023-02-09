module Croq.Ui.Tab exposing (..)

{-| A topdown vision of a Map with some highlighted points.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Ev
import List.Extra
import Croq.Ui.Color exposing (..)


type alias Config a msg =
    { message : Msg -> msg
    , tabs : List ( String, a -> Html msg )
    }


type alias Model =
    { index : Int, size : Int }


type Msg
    = OnSelect Int
    | OnLeft
    | OnRight


update : Msg -> Model -> Model
update msg m =
    case msg of
        OnSelect i ->
            { m | index = Basics.max 0 (Basics.min (m.size - 1) i) }

        OnLeft ->
            { m | index = modBy m.size (m.index - 1) }

        OnRight ->
            { m | index = modBy m.size (m.index + 1) }


init : Config a msg -> Model
init cfg =
    { index = 0, size = List.length cfg.tabs }


view : Config a msg -> Model -> a -> Html msg
view cfg tab m =
    let
        ( tabNames, tabViews ) =
            List.unzip cfg.tabs

        viewFunction =
            List.drop tab.index tabViews
                |> List.head
                |> Maybe.withDefault (\_ -> div [] [])
    in
    div []
        [ div [ class "tabs my-4 w-full text-lg font-bold w-100 flex justify-center space-x-2" ] (List.indexedMap (viewTab cfg tab.index) tabNames)
        , viewFunction m
        ]


viewTab : { a | message : Msg -> msg } -> Int -> Int -> String -> Html msg
viewTab cfg select idx name =
    let
        tabClass =
            if select == idx then
                " tab flex-1 tab-bordered tab-active"

            else
                "tab flex-1 tab-bordered"
    in
    Html.map cfg.message <|
        button
            [ class tabClass, Ev.onClick (OnSelect idx) ]
            [ text name ]


config : (Msg -> msg) -> List ( String, a -> Html msg ) -> Config a msg
config onmsg tabs =
    Config onmsg tabs


setIndex : Int -> Model -> Model
setIndex idx m =
    update (OnSelect idx) m


getIndex : Model -> Int
getIndex m =
    m.index


getTitle : Config a msg -> Model -> String
getTitle cfg m =
    List.Extra.getAt m.index cfg.tabs
        |> Maybe.map Tuple.first
        |> Maybe.withDefault ""
