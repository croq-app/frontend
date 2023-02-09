module Croq.Data.Region exposing (LocatedBoulderFormation, LocatedBoulderProblem, LocatedElem, LocatedRoute, LocatedSector, Region, breadcrumbs, decoder, encoder, getBoulderFormation, getBoulderProblem, getRoute, getSector, locatedBoulderFormationBreadcrumbs, locatedProblemBreadcrumbs, locatedRouteBreadcrumbs, locatedSectorBreadcrumbs, locatedSectorDecoder, locatedSectorEncoder, sectors)

import Croq.Data.Attraction as Attraction exposing (Attraction)
import Croq.Data.BoulderFormation exposing (BoulderFormation)
import Croq.Data.BoulderProblem exposing (BoulderProblem)
import Croq.Data.Id as Id
import Croq.Data.Loading as Loading exposing (LoadingHttp)
import Croq.Data.Route exposing (Route)
import Croq.Data.Sector as Sector exposing (Sector)
import Croq.Data.Types exposing (..)
import Croq.Routes as Routes
import Json.Decode as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import LatLng as LatLng exposing (LatLng)


type alias Region =
    { id : Id.RegionId
    , name : Name
    , country : Country
    , location : LatLng
    , description : Text
    , howToAccess : Text
    , attractions : List Attraction
    , sectors : List Sector
    }


type alias LocatedSector =
    { region : Region
    , sector : Sector
    }


type alias LocatedRoute =
    { region : Region
    , sector : Sector
    , elem : Route
    }


type alias LocatedBoulderFormation =
    { region : Region
    , sector : Sector
    , elem : BoulderFormation
    }


type alias LocatedElem elem =
    { region : Region
    , sector : Sector
    , elem : elem
    }


type alias LocatedBoulderProblem =
    { region : Region
    , sector : Sector
    , elem : BoulderFormation
    , problem : BoulderProblem
    }


getBoulderProblem : Id.ProblemId -> LocatedBoulderFormation -> Maybe LocatedBoulderProblem
getBoulderProblem id { region, sector, elem } =
    Id.elemFromId id elem.problems
        |> Maybe.map (LocatedBoulderProblem region sector elem)


getRoute : Id.ElemId -> LocatedSector -> Maybe LocatedRoute
getRoute id { region, sector } =
    Id.elemFromId id sector.routes
        |> Maybe.map (LocatedRoute region sector)


getBoulderFormation : Id.ElemId -> LocatedSector -> Maybe LocatedBoulderFormation
getBoulderFormation id { region, sector } =
    Id.elemFromId id sector.boulders
        |> Maybe.map (LocatedBoulderFormation region sector)


getSector : Id.SectorId -> Region -> Maybe LocatedSector
getSector id region =
    Id.elemFromId id region.sectors
        |> Maybe.map (LocatedSector region)


toElem : LocatedBoulderProblem -> LocatedElem BoulderFormation
toElem { region, sector, elem } =
    LocatedElem region sector elem


toSector : LocatedElem a -> LocatedSector
toSector { region, sector } =
    LocatedSector region sector


toRegion : LocatedSector -> Region
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
        , ( Routes.regionUrl m.id, Loading.unwrap m.id.slug .name m.data )
        ]

    else
        [ ( Routes.regionUrl m.id, Loading.unwrap m.id.slug .name m.data ) ]


locatedSectorBreadcrumbs : { m | id : Id.SectorId, data : LoadingHttp LocatedSector } -> List ( Url, Name )
locatedSectorBreadcrumbs m =
    breadcrumbs { id = m.id.parent, data = Loading.map toRegion m.data }
        ++ [ ( Routes.boulderSectorUrl m.id
             , Loading.unwrap m.id.slug (.sector >> .name) m.data
             )
           ]


locatedBoulderFormationBreadcrumbs : { m | id : Id.ElemId, data : LoadingHttp LocatedBoulderFormation } -> List ( Url, Name )
locatedBoulderFormationBreadcrumbs m =
    locatedSectorBreadcrumbs { id = m.id.parent, data = Loading.map toSector m.data }
        ++ [ ( Routes.boulderFormationUrl m.id
             , Loading.unwrap m.id.slug (.elem >> .name) m.data
             )
           ]


locatedRouteBreadcrumbs : { m | id : Id.ElemId, data : LoadingHttp LocatedRoute } -> List ( Url, Name )
locatedRouteBreadcrumbs m =
    locatedSectorBreadcrumbs { id = m.id.parent, data = Loading.map toSector m.data }
        ++ [ ( Routes.routeUrl m.id
             , Loading.unwrap m.id.slug (.elem >> .name) m.data
             )
           ]


locatedProblemBreadcrumbs : { m | id : Id.ProblemId, data : LoadingHttp LocatedBoulderProblem } -> List ( Url, Name )
locatedProblemBreadcrumbs m =
    locatedBoulderFormationBreadcrumbs { id = m.id.parent, data = Loading.map toElem m.data }
        ++ [ ( Routes.boulderProblemUrl m.id
             , Loading.unwrap m.id.slug (.problem >> .name) m.data
             )
           ]


sectors : Region -> List LocatedSector
sectors region =
    region.sectors |> List.map (LocatedSector region)


decoder : D.Decoder Region
decoder =
    D.succeed Region
        |> D.required "id" Id.decodeRegionId
        |> D.required "name" D.string
        |> D.required "country" D.string
        |> D.required "location" LatLng.decoder
        |> D.optional "description" D.string ""
        |> D.optional "how_to_access" D.string ""
        |> D.optional "attractions" (D.list Attraction.decoder) []
        |> D.optional "sectors" (D.list Sector.decoder) []


locatedSectorDecoder : D.Decoder LocatedSector
locatedSectorDecoder =
    D.succeed LocatedSector
        |> D.required "region" decoder
        |> D.required "sector" Sector.decoder


encoder : Region -> E.Value
encoder region =
    E.object
        [ ( "id", Id.encodeRegionId region.id )
        , ( "name", E.string region.name )
        , ( "country", E.string region.country )
        , ( "location", LatLng.encoder region.location )
        , ( "description", E.string region.description )
        , ( "how_to_access", E.string region.howToAccess )
        , ( "attractions", E.list Attraction.encoder region.attractions )
        , ( "sectors", E.list Sector.encoder region.sectors )
        ]


locatedSectorEncoder : LocatedSector -> E.Value
locatedSectorEncoder loc =
    E.object
        [ ( "region", encoder loc.region )
        , ( "sector", Sector.encoder loc.sector )
        ]
