module Croq.Pages.SimplePage exposing (Model, Msg, init, update, view)

{-| Example bare bones page
-}

import Croq.Config as Cfg
import Croq.Ui as Ui
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


view : Cfg.Model -> Model -> Html Msg
view _ _ =
    Ui.appShell <|
        div [] [ text "Example page, Nothing to see here" ]
