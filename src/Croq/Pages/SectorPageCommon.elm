module Croq.Pages.SectorPageCommon exposing (..)

import Croq.Api as Api
import Croq.Config as Cfg
import Croq.Data.Id exposing (..)
import Croq.Data.Loading as Loading exposing (LoadingHttp)
import Croq.Data.Region as Region exposing (LocatedSector)
import Croq.Data.Types exposing (..)
import Croq.Ui as Ui
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http


viewAccess : { m | data : LoadingHttp LocatedSector } -> Html msg
viewAccess m =
    Ui.viewLoading m.data <|
        \{ sector } ->
            div []
                [ p [] [ text sector.howToAccess ] ]


sectorGet : (b -> c) -> c -> { d | data : LoadingHttp { a | sector : b } } -> c
sectorGet =
    dataGet .sector


dataGet : (a -> b) -> (b -> c) -> c -> { d | data : LoadingHttp a } -> c
dataGet project attr default m =
    Loading.unwrap default (project >> attr) m.data


httpDataRequest : (Result Http.Error LocatedSector -> msg) -> Cfg.Model -> SectorId -> Cmd msg
httpDataRequest msg cfg id =
    Http.get
        { url = Api.sectorUrl cfg id
        , expect = Http.expectJson msg Region.locatedSectorDecoder
        }