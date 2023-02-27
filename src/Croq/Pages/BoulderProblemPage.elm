module Croq.Pages.BoulderProblemPage exposing (Model, Msg, entry, update, view, subscriptions)

import Croq.Config as Cfg
import Croq.Data.BoulderProblem as BoulderProblem
import Croq.Data.Id exposing (..)
import Croq.Data.Loading as Loading exposing (LoadingHttp)
import Croq.Data.Region as Region exposing (SectorCur)
import Croq.Data.Types exposing (..)
import Croq.Pages.SectorPageCommon exposing (httpDataRequest)
import Croq.Ui as Ui
import Croq.Ui.Carousel as Carousel
import Croq.Ui.Climbable as Climbable
import Daisy.Elements as Ui
import Html exposing (..)
import Html.Attributes exposing (..)
import Http


type alias Model =
    { id : ProblemId
    , data : LoadingHttp Region.BoulderProblemCur
    }


type Msg
    = OnDataReceived (Result Http.Error SectorCur)


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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Cfg.Model -> Model -> Html Msg
view _ m =
    Ui.appShell <|
        Ui.viewLoading m.data <|
            \{ problem, elem } ->
                Ui.container
                    [ Ui.breadcrumbs (Region.problemBreadcrumbs m)
                    , Ui.title problem.name
                    , Ui.tags (BoulderProblem.tags problem)
                    , Carousel.view carouselConfig [ "??", "??" ]
                    , Ui.sections [] (( "Bloco", [ text elem.name ] ) :: Climbable.sections problem)
                    ]


carouselConfig : Carousel.Config String msg
carouselConfig =
    Carousel.config
