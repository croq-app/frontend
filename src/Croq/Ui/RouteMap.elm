module Croq.Ui.RouteMap exposing (..)

{-| A topdown vision of a Map with some highlighted points. Follows TEA.
-}

import Croq.Ui.Color exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    ()


type alias Msg =
    ()


view : Model -> Html msg
view _ =
    div [ class "bg-gradient-to-r from-cyan-500 to-blue-500 my-4 h-48" ] [ text "TODO: Topdown view of boulders in a sector" ]


update : Msg -> Model -> Model
update _ m =
    m


init : Model
init =
    ()
