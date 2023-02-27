module Croq.Pages.TopoToolPage exposing (Model, Msg, config, init, subscriptions, update, view)

{-| Example bare bones page
-}

import Croq.Config as Cfg
import Croq.Ui exposing (appShell)
import Daisy.Elements as Ui
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg.Editor as Editor
import Svg.Editor.Config as EditorCfg


type Msg
    = NoOp
    | OnEditorMsg Editor.Msg


type alias Model =
    { editor : Editor.Model
    }


config : Editor.Config
config =
    EditorCfg.config []


init : Model
init =
    { editor = Editor.init }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg_ m =
    let
        return m_ =
            ( m_, Cmd.none )
    in
    case msg_ of
        NoOp ->
            return m

        OnEditorMsg msg ->
            let
                ( new, cmd ) =
                    Editor.update config msg m.editor
            in
            ( { m | editor = new }, Cmd.map OnEditorMsg cmd )


subscriptions : Model -> Sub Msg
subscriptions m =
    Sub.map OnEditorMsg <| Editor.subscriptions config m.editor


view : Cfg.Model -> Model -> Html Msg
view _ m =
    (appShell << Ui.container) <|
        [ Ui.title "Editor de croquis"
        , Html.map OnEditorMsg <| Editor.view config m.editor
        ]
