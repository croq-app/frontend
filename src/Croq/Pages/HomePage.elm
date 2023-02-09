module Croq.Pages.HomePage exposing (Model, Msg, entry, update, view)

import Croq.Config as Cfg
import Croq.Data.Id exposing (..)
import Croq.Routes as Routes
import Croq.Ui as Ui
import Html exposing (..)
import Html.Attributes exposing (..)


type Model
    = NotImpl


type alias Msg =
    ()


entry : Cfg.Model -> ( Model, Cmd a )
entry _ =
    ( NotImpl, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update _ m =
    ( m, Cmd.none )


view : Model -> Html Msg
view _ =
    Ui.appShell <|
        div []
            [ div
                [ class "hero min-h-screen with-navbar"
                , style "background-image" "url(/static/hero-image.png)"
                ]
                [ div
                    [ class "hero-overlay bg-opacity-30" ]
                    []
                , div
                    [ class "hero-content text-center text-white"
                    ]
                    [ div
                        [ class "max-w-md" ]
                        [ h1 [ class "mb-5 text-5xl font-bold" ] [ text "faaala, lek!" ]
                        , p [ class "mb-5" ]
                            [ text "Provident cupiditate voluptatem et in. Quaerat fugiat ut assumenda excepturi exercitationem quasi. In deleniti eaque aut repudiandae et a id nisi." ]
                        , div [ class "btn-group" ]
                            [ a
                                [ class "btn btn-secondary"
                                , class "hover:ring"
                                , href (Routes.regionUrl (regionId "br" "belchi"))
                                ]
                                [ text "Ir para Belchi" ]
                            , a
                                [ class "btn btn-primary"
                                , class "hover:ring"
                                , href (Routes.regionUrl (regionId "br" "cocal"))
                                ]
                                [ text "Ir para Cocal" ]
                            ]
                        ]
                    ]
                ]
            ]
