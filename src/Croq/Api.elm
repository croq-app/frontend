module Croq.Api exposing (..)

import Croq.Config as Cfg
import Croq.Data.Id exposing (..)
import Croq.Data.Types exposing (..)
import String.Extra as String


url : Cfg.Model -> String -> Url
url cfg rest =
    cfg.api ++ String.unsurround "/" rest


regionUrl : Cfg.Model -> RegionId -> Url
regionUrl cfg id =
    url cfg <| showRegionId id ++ ".json"


sectorUrl : Cfg.Model -> SectorId -> Url
sectorUrl cfg id =
    url cfg <| showSectorId id ++ ".json"


static : Cfg.Model -> String -> String
static _ part =
    "/static/" ++ part
