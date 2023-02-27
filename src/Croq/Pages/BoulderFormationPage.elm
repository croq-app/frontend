module Croq.Pages.BoulderFormationPage exposing (Model, Msg, entry, subscriptions, update, view)

import Croq.Config as Cfg
import Croq.Data.BoulderFormation exposing (BoulderFormation)
import Croq.Data.BoulderProblem exposing (BoulderProblem, showGrade)
import Croq.Data.Id exposing (..)
import Croq.Data.Loading as Loading exposing (LoadingHttp)
import Croq.Data.Region as Region exposing (SectorCur)
import Croq.Data.Types exposing (..)
import Croq.Pages.SectorPageCommon exposing (httpDataRequest)
import Croq.Routes as Routes
import Croq.Ui as Ui
import Croq.Ui.Carousel as Carousel
import Daisy.Accordion as Accordion
import Daisy.Elements as Ui
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy)
import Http
import Markdown


type alias Model =
    { id : ElemId
    , data : LoadingHttp (Region.ElemCur BoulderFormation)
    , accordion : Accordion.State
    }


type Msg
    = OnDataReceived (Result Http.Error SectorCur)
    | OnAccordionUpdate Accordion.State


entry : Cfg.Model -> ElemId -> ( Model, Cmd Msg )
entry cfg id =
    ( { id = id
      , data = Loading.Pending
      , accordion = Accordion.init
      }
    , httpDataRequest OnDataReceived cfg id.parent
    )


update : Cfg.Model -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg m =
    let
        return m_ =
            ( m_, Cmd.none )
    in
    case msg of
        OnDataReceived data ->
            return
                { m
                    | data =
                        Loading.fromResult data
                            |> Loading.andThen
                                (Region.getBoulderFormation m.id
                                    >> Loading.fromMaybe "Bloco não encontrado"
                                    >> Loading.toHttp
                                )
                }

        OnAccordionUpdate state ->
            return { m | accordion = state }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Cfg.Model -> Model -> Html Msg
view _ m =
    let
        info =
            List.map (\x -> ( m, x )) (Loading.unwrap [] (.elem >> .problems) m.data)
    in
    Ui.appShell <|
        Ui.container
            [ Ui.breadcrumbs (Region.boulderFormationBreadcrumbs m)
            , Ui.title "Bloco do Fax"
            , Carousel.view carouselConfig [ "foo", "bar", "baz" ]
            , Accordion.view accordionConfig m.accordion info
            ]


accordionConfig : Accordion.Config ( Model, BoulderProblem ) Msg
accordionConfig =
    Accordion.config
        OnAccordionUpdate
        (\( _, problem ) -> problem.name ++ " (" ++ showGrade problem ++ ")")
        (\( _, problem ) ->
            div []
                [ dl []
                    [ dt [] [ text "Grau:" ]
                    , dd [] [ text (showGrade problem) ]
                    , dt [] [ text "Descrição:" ]
                    , dd [] [ lazy (Markdown.toHtml []) problem.description ]
                    ]
                , a
                    [ href (Routes.boulderProblemUrl problem.id)
                    , class "btn glass w-full btn-accent"
                    ]
                    [ text "Ver detalhes" ]
                ]
        )


carouselConfig : Carousel.Config String msg
carouselConfig =
    Carousel.config
