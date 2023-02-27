module Croq.Pages.ParkingPage exposing (Model, Msg, entry, subscriptions, update, view)

import Croq.Data.Id exposing (..)
import Croq.Data.Types exposing (..)
import Croq.Ui as Ui
import Html exposing (..)
import Html.Attributes exposing (..)


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


view : Model -> Html Msg
view _ =
    Ui.appShell <|
        div [] [ text "TODO: parking" ]
