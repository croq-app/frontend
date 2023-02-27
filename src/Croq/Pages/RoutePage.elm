module Croq.Pages.RoutePage exposing (Model, Msg, entry, update, view, subscriptions)

import Croq.Config as Cfg
import Croq.Data.Id exposing (..)
import Croq.Data.Loading as Loading exposing (LoadingHttp)
import Croq.Data.Region as Region exposing (SectorCur)
import Croq.Data.Route as Route
import Croq.Data.Types exposing (..)
import Croq.Pages.SectorPageCommon exposing (httpDataRequest)
import Croq.Ui as Ui
import Croq.Ui.Carousel as Carousel
import Daisy.Elements as Ui
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy)
import Http
import Markdown


type alias Model =
    { id : ElemId
    , data : LoadingHttp Region.RouteCur
    }


type Msg
    = OnDataReceived (Result Http.Error SectorCur)


entry : Cfg.Model -> ElemId -> ( Model, Cmd Msg )
entry cfg id =
    ( { id = id
      , data = Loading.Pending
      }
    , httpDataRequest OnDataReceived cfg id.parent
    )


update : Msg -> Model -> Model
update msg m =
    case msg of
        OnDataReceived data ->
            { m
                | data =
                    Loading.fromResult data
                        |> Loading.andThen
                            (Region.getRoute m.id
                                >> Loading.fromMaybe "Via não encontrada"
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
            \{ elem } ->
                Ui.container
                    [ Ui.breadcrumbs (Region.routeBreadcrumbs m)
                    , Ui.title elem.name
                    , Ui.tags (Route.tags elem)
                    , Carousel.view carouselConfig [ "??", "??" ]
                    , Ui.sections []
                        [ ( "Descrição", [ lazy (Markdown.toHtml []) elem.description ] )
                        , ( "Vídeos"
                          , [ Ui.urlList [ class "list-disc pl-6" ] ( "Vazio", List.map (\x -> ( x, x )) elem.videos ) ]
                          )
                        ]
                    ]


carouselConfig : Carousel.Config String msg
carouselConfig =
    Carousel.config
