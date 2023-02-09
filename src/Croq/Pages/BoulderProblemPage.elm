module Croq.Pages.BoulderProblemPage exposing (Model, Msg, entry, update, view)

import Croq.Config as Cfg
import Croq.Data.BoulderProblem as BoulderProblem
import Croq.Data.Id exposing (..)
import Croq.Data.Loading as Loading exposing (LoadingHttp)
import Croq.Data.Region as Region exposing (LocatedSector)
import Croq.Data.Types exposing (..)
import Croq.Pages.SectorPageCommon exposing (httpDataRequest)
import Croq.Ui as Ui
import Croq.Ui.Carousel as Carousel
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy)
import Http
import Markdown


type alias Model =
    { id : ProblemId
    , data : LoadingHttp Region.LocatedBoulderProblem
    }


type Msg
    = OnDataReceived (Result Http.Error LocatedSector)


entry : Cfg.Model -> ProblemId -> ( Model, Cmd Msg )
entry cfg id =
    ( { id = id
      , data = Loading.Pending
      }
    , httpDataRequest OnDataReceived cfg id.parent.parent
    )


update : Msg -> Model -> Model
update msg m =
    case msg of
        OnDataReceived data ->
            { m
                | data =
                    Loading.fromResult data
                        |> Loading.andThen
                            (Region.getBoulderFormation m.id.parent
                                >> Maybe.andThen (Region.getBoulderProblem m.id)
                                >> Loading.fromMaybe "Problema não encontrado"
                                >> Loading.toHttp
                            )
            }


view : Cfg.Model -> Model -> Html Msg
view _ m =
    Ui.appShell <|
        Ui.viewLoading m.data <|
            \{ problem, elem } ->
                Ui.container
                    [ Ui.breadcrumbs (Region.locatedProblemBreadcrumbs m)
                    , Ui.title problem.name
                    , Ui.tags (BoulderProblem.tags problem)
                    , Carousel.view carouselConfig [ "??", "??" ]
                    , Ui.sections
                        [ ( "Bloco", [ text elem.name ] )
                        , ( "Descrição/Saída", [ lazy (Markdown.toHtml []) problem.description ] )
                        , ( "Vídeos"
                          , [ Ui.urlList "Vazio" [ class "list-disc pl-6" ] (List.map (\x -> ( x, x )) problem.videos) ]
                          )
                        ]
                    ]


carouselConfig : Carousel.Config String msg
carouselConfig =
    Carousel.config
