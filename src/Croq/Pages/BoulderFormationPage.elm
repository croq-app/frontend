module Croq.Pages.BoulderFormationPage exposing (Model, Msg, entry, update, view)

import Croq.Config as Cfg
import Croq.Data.BoulderFormation exposing (BoulderFormation)
import Croq.Data.BoulderProblem exposing (BoulderProblem, showGrade)
import Croq.Data.Id exposing (..)
import Croq.Data.Loading as Loading exposing (LoadingHttp)
import Croq.Data.Region as Region exposing (LocatedSector)
import Croq.Data.Types exposing (..)
import Croq.Pages.SectorPageCommon exposing (httpDataRequest)
import Croq.Routes as Routes
import Croq.Ui as Ui
import Croq.Ui.Accordion as Accordion
import Croq.Ui.Carousel as Carousel
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy)
import Http
import Markdown


type alias Model =
    { id : ElemId
    , data : LoadingHttp (Region.LocatedElem BoulderFormation)
    , accordion : Accordion.State
    }


type Msg
    = OnDataReceived (Result Http.Error LocatedSector)
    | OnAccordionUpdate Accordion.State


entry : Cfg.Model -> ElemId -> ( Model, Cmd Msg )
entry cfg id =
    ( { id = id
      , data = Loading.Pending
      , accordion = Accordion.init
      }
    , httpDataRequest OnDataReceived cfg id.parent
    )


update : Msg -> Cfg.Model -> Model -> ( Model, Cmd Msg )
update msg _ m =
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


view : Cfg.Model -> Model -> Html Msg
view _ m =
    let
        info =
            List.map (\x -> ( m, x )) (Loading.unwrap [] (.elem >> .problems) m.data)
    in
    Ui.appShell <|
        Ui.container
            [ Ui.breadcrumbs (Region.locatedBoulderFormationBreadcrumbs m)
            , Ui.title "Bloco do Fax"
            , Carousel.view carouselConfig [ "foo", "bar", "baz" ]
            , Accordion.view accordionConfig m.accordion info
            ]


accordionConfig : Accordion.Config ( Model, BoulderProblem ) Msg
accordionConfig =
    Accordion.config
        { toMsg = OnAccordionUpdate
        , render =
            \( _, problem ) ->
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
        , title = \( _, problem ) -> problem.name ++ " (" ++ showGrade problem ++ ")"
        }


carouselConfig : Carousel.Config String msg
carouselConfig =
    Carousel.config