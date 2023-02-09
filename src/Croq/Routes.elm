module Croq.Routes exposing
    ( Route(..)
    , boulderFormationUrl
    , boulderProblemUrl
    , boulderSectorUrl
    , countryUrl
    , homeUrl
    , parkingUrl
    , parseUrl
    , regionUrl
    , routeSectorUrl
    , routeUrl
    , sectorUrl
    , toHref
    )

-- import Croq.Data.Types exposing (..)

import Croq.Data.Id exposing (..)
import Croq.Data.Sector exposing (Kind(..))
import Croq.Util exposing (iff)
import Url exposing (Url)
import Url.Parser as P exposing ((</>))


type Route
    = Home
    | Error String
    | Region RegionId
    | BoulderSector SectorId
    | BoulderFormation ElemId
    | RouteSector SectorId
    | Route ElemId
    | BoulderProblem ProblemId
    | Parking SectorId
    | GradeTool
    | GpsTool
    | TopoTool
    | Playground


boulderSlug : String
boulderSlug =
    "boulder"


routeSlug : String
routeSlug =
    "r"


parkingSlug : String
parkingSlug =
    "p"


gradeToolSlug : String
gradeToolSlug =
    "grades"


cGpsToolSlug : String
cGpsToolSlug =
    "gps"


topoToolSlug : String
topoToolSlug =
    "topo"


playgroundSlug : String
playgroundSlug =
    "playground"


matchers : P.Parser (Route -> a) a
matchers =
    let
        slug =
            P.string

        wrap =
            { f2 = \f g x y -> f (g x y)
            , f3 = \f g x y z -> f (g x y z)
            , f4 = \f g x y z w -> f (g x y z w)
            , f5 = \f g x y z w r -> f (g x y z w r)
            }
    in
    P.oneOf
        [ P.map Home (P.s "frontend") -- redirect from github-pages
        , P.map Home P.top

        --- Generic pages
        , P.map GradeTool (P.s gradeToolSlug)
        , P.map GpsTool (P.s cGpsToolSlug)
        , P.map TopoTool (P.s topoToolSlug)
        , P.map Playground (P.s playgroundSlug)

        --- URLS associated with specific regions
        , P.map (wrap.f2 Region regionId) (slug </> slug)
        , P.map (wrap.f3 BoulderSector sectorId) (slug </> slug </> P.s boulderSlug </> slug)
        , P.map (wrap.f4 BoulderFormation elemId) (slug </> slug </> P.s boulderSlug </> slug </> slug)
        , P.map (wrap.f5 BoulderProblem problemId) (slug </> slug </> P.s boulderSlug </> slug </> slug </> slug)
        , P.map (wrap.f3 RouteSector sectorId) (slug </> slug </> P.s routeSlug </> slug)
        , P.map (wrap.f4 Route elemId) (slug </> slug </> P.s routeSlug </> slug </> slug)
        , P.map (wrap.f3 Parking sectorId) (slug </> slug </> P.s parkingSlug </> slug)
        ]


parseUrl : Url -> Route
parseUrl url =
    case P.parse matchers url of
        Just route ->
            route

        Nothing ->
            Error (Url.toString url)


toHref : Route -> String
toHref route =
    let
        mk inner path =
            case String.split "/" path of
                [ _, _ ] ->
                    "/" ++ path ++ "/"

                a :: b :: rest ->
                    "/" ++ a ++ "/" ++ b ++ iff (inner == "") "" ("/" ++ inner ++ "/") ++ String.join "/" rest ++ "/"

                _ ->
                    "/" ++ path ++ "/"
    in
    case route of
        Home ->
            "/"

        Region id ->
            mk "" (showRegionId id)

        BoulderSector id ->
            mk boulderSlug (showSectorId id)

        BoulderFormation id ->
            mk boulderSlug (showElemId id)

        BoulderProblem id ->
            mk boulderSlug (showProblemId id)

        RouteSector id ->
            mk routeSlug (showSectorId id)

        Route id ->
            mk routeSlug (showElemId id)

        Parking id ->
            mk parkingSlug (showSectorId id)

        GpsTool ->
            mk "" cGpsToolSlug

        GradeTool ->
            mk "" gradeToolSlug

        TopoTool ->
            mk "" topoToolSlug

        Playground ->
            mk "" playgroundSlug

        Error st ->
            "/error/" ++ st


homeUrl : String
homeUrl =
    toHref Home


countryUrl : CountryId -> String
countryUrl id =
    "/" ++ id.slug


regionUrl : RegionId -> String
regionUrl id =
    toHref (Region id)


boulderSectorUrl : SectorId -> String
boulderSectorUrl id =
    toHref (BoulderSector id)


sectorUrl : Kind -> SectorId -> String
sectorUrl kind id =
    case kind of
        Climbing ->
            routeSectorUrl id

        Bouldering ->
            boulderSectorUrl id


boulderFormationUrl : ElemId -> String
boulderFormationUrl id =
    toHref (BoulderFormation id)


boulderProblemUrl : ProblemId -> String
boulderProblemUrl id =
    toHref (BoulderProblem id)


routeSectorUrl : SectorId -> String
routeSectorUrl id =
    toHref (RouteSector id)


routeUrl : ElemId -> String
routeUrl id =
    toHref (Route id)


parkingUrl : SectorId -> String
parkingUrl id =
    toHref (Parking id)
