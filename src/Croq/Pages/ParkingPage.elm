module Croq.Pages.ParkingPage exposing (Model, Msg, entry, subscriptions, update, view)

import Croq.Config as Cfg
import Croq.Data.Id exposing (..)
import Croq.Data.Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Daisy.Elements as Ui


type Model
    = NotImpl


type Msg
    = NoOp


entry : SectorId -> ( Model, Cmd a )
entry _ =
    ( NotImpl, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            ( m, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Cfg.Model -> Model -> Html Msg
view _ _ =
    Ui.container [ text "TODO: parking" ]
