module Croq.Ui.RouteMap exposing (..)

{-| A topdown vision of a Map with some highlighted points. Follows TEA.
-}

import Croq.Ui.Color exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg.Editor as Editor
import Svg.Editor.Config as EditorCfg


type alias Model =
    Editor.Model


type alias Msg =
    Editor.Msg


config : Editor.Config
config =
    EditorCfg.config []


view : Model -> Html Msg
view m =
    Editor.view config m


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    Editor.update config msg m


init : Model
init =
    Editor.init
