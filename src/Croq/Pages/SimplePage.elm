module Croq.Pages.SimplePage exposing (Model, Msg, init, update, view, subscriptions)

{-| Example bare bones page
-}

import Croq.Config as Cfg
import Croq.Ui as Ui
import Html exposing (..)
import Html.Attributes exposing (..)


type Model
    = NotImpl


type Msg
    = NoOp


init : Model
init =
    NotImpl


update : Msg -> Cfg.Model -> Model -> ( Model, Cmd Msg )
update msg _ m =
    case msg of
        NoOp ->
            ( m, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Cfg.Model -> Model -> Html Msg
view cfg _ =
    Ui.appShell cfg <|
        div [] [ text "Example page, Nothing to see here" ]
