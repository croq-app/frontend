module Croq.Data.Region exposing (BoulderFormationCur, BoulderProblemCur, ElemCur, Region, RouteCur, SectorCur, boulderFormationBreadcrumbs, breadcrumbs, decoder, encoder, getBoulderFormation, getBoulderProblem, getRoute, getSector, locatedSectorDecoder, locatedSectorEncoder, problemBreadcrumbs, routeBreadcrumbs, sectorBreadcrumbs, sectors)

import Croq.Data.Attraction as Attraction exposing (Attraction)
import Croq.Data.BoulderFormation exposing (BoulderFormation)
import Croq.Data.BoulderProblem exposing (BoulderProblem)
import Croq.Data.Decode as D
import Croq.Data.Encode as E
import Croq.Data.Id as Id
import Croq.Data.Loading as Loading exposing (LoadingHttp)
import Croq.Data.Route exposing (Route)
import Croq.Data.Sector as Sector exposing (Sector)
import Croq.Data.Types exposing (..)
import Croq.Data.Util exposing (normalizeShortName)
import Croq.Routes as Routes
import Json.Decode as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import LatLng as LatLng exposing (LatLng)


type alias Region =
    { id : Id.RegionId
    , name : Name
    , shortName : Name
    , country : Country
    , location : LatLng
    , description : Maybe RichText
    , howToAccess : Maybe RichText
    , attractions : List Attraction
    , sectors : List Sector
    }


type alias SectorCur =
    { region : Region
    , sector : Sector
    }


type alias RouteCur =
    { region : Region
    , sector : Sector
    , elem : Route
    }


type alias BoulderFormationCur =
    { region : Region
    , sector : Sector
    , elem : BoulderFormation
    }


type alias ElemCur elem =
    { region : Region
    , sector : Sector
    , elem : elem
    }


type alias BoulderProblemCur =
    { region : Region
    , sector : Sector
    , elem : BoulderFormation
    , problem : BoulderProblem
    }


getBoulderProblem : Id.ProblemId -> BoulderFormationCur -> Maybe BoulderProblemCur
getBoulderProblem id { region, sector, elem } =
    Id.elemFromId id elem.problems
        |> Maybe.map (BoulderProblemCur region sector elem)


getRoute : Id.ElemId -> SectorCur -> Maybe RouteCur
getRoute id { region, sector } =
    Id.elemFromId id sector.routes
        |> Maybe.map (RouteCur region sector)


getBoulderFormation : Id.ElemId -> SectorCur -> Maybe BoulderFormationCur
getBoulderFormation id { region, sector } =
    Id.elemFromId id sector.boulders
        |> Maybe.map (BoulderFormationCur region sector)


getSector : Id.SectorId -> Region -> Maybe SectorCur
getSector id region =
    Id.elemFromId id region.sectors
        |> Maybe.map (SectorCur region)


toElem : BoulderProblemCur -> ElemCur BoulderFormation
toElem { region, sector, elem } =
    ElemCur region sector elem


toSector : ElemCur a -> SectorCur
toSector { region, sector } =
    SectorCur region sector


toRegion : SectorCur -> Region
toRegion { region } =
    region


breadcrumbs : { m | id : Id.RegionId, data : LoadingHttp Region } -> List ( Url, Name )
breadcrumbs m =
    if False then
        let
            countryCode =
                m.id.parent.slug |> String.toUpper
        in
        [ ( Routes.countryUrl m.id.parent, countryCode )
        , ( Routes.regionUrl m.id, Loading.unwrap m.id.slug .shortName m.data )
        ]

    else
        [ ( Routes.regionUrl m.id, Loading.unwrap m.id.slug .shortName m.data ) ]


sectorBreadcrumbs : { m | id : Id.SectorId, data : LoadingHttp SectorCur } -> List ( Url, Name )
sectorBreadcrumbs m =
    breadcrumbs { id = m.id.parent, data = Loading.map toRegion m.data }
        ++ [ ( Routes.boulderSectorUrl m.id
             , Loading.unwrap m.id.slug (.sector >> .shortName) m.data
             )
           ]


boulderFormationBreadcrumbs : { m | id : Id.ElemId, data : LoadingHttp BoulderFormationCur } -> List ( Url, Name )
boulderFormationBreadcrumbs m =
    sectorBreadcrumbs { id = m.id.parent, data = Loading.map toSector m.data }
        ++ [ ( Routes.boulderFormationUrl m.id
             , Loading.unwrap m.id.slug (.elem >> .shortName) m.data
             )
           ]


routeBreadcrumbs : { m | id : Id.ElemId, data : LoadingHttp RouteCur } -> List ( Url, Name )
routeBreadcrumbs m =
    sectorBreadcrumbs { id = m.id.parent, data = Loading.map toSector m.data }
        ++ [ ( Routes.routeUrl m.id
             , Loading.unwrap m.id.slug (.elem >> .shortName) m.data
             )
           ]


problemBreadcrumbs : { m | id : Id.ProblemId, data : LoadingHttp BoulderProblemCur } -> List ( Url, Name )
problemBreadcrumbs m =
    boulderFormationBreadcrumbs { id = m.id.parent, data = Loading.map toElem m.data }
        ++ [ ( Routes.boulderProblemUrl m.id
             , Loading.unwrap m.id.slug (.problem >> .shortName) m.data
             )
           ]


sectors : Region -> List SectorCur
sectors region =
    region.sectors |> List.map (SectorCur region)


decoder : D.Decoder Region
decoder =
    D.succeed Region
        |> D.required "id" Id.decodeRegionId
        |> D.required "name" D.string
        |> D.optional "short_name" D.string ""
        |> D.required "country" D.string
        |> D.required "location" LatLng.decoder
        |> D.nullableField "description" D.richText
        |> D.nullableField "how_to_access" D.richText
        |> D.optional "attractions" (D.list Attraction.decoder) []
        |> D.optional "sectors" (D.list Sector.decoder) []
        |> D.map normalizeShortName


locatedSectorDecoder : D.Decoder SectorCur
locatedSectorDecoder =
    D.succeed SectorCur
        |> D.required "region" decoder
        |> D.required "sector" Sector.decoder


encoder : Region -> E.Value
encoder region =
    E.object
        [ ( "id", Id.encodeRegionId region.id )
        , ( "name", E.string region.name )
        , ( "short_name", E.string region.shortName )
        , ( "country", E.string region.country )
        , ( "location", LatLng.encoder region.location )
        , ( "description", E.nullable E.richText region.description )
        , ( "how_to_access", E.nullable E.richText region.howToAccess )
        , ( "attractions", E.list Attraction.encoder region.attractions )
        , ( "sectors", E.list Sector.encoder region.sectors )
        ]


locatedSectorEncoder : SectorCur -> E.Value
locatedSectorEncoder loc =
    E.object
        [ ( "region", encoder loc.region )
        , ( "sector", Sector.encoder loc.sector )
        ]
