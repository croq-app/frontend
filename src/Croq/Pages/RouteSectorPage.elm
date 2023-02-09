module Croq.Pages.RouteSectorPage exposing (Model, Msg, entry, update, view)

import Croq.Config as Cfg
import Croq.Data.Id exposing (..)
import Croq.Data.Loading as Loading exposing (LoadingHttp)
import Croq.Data.Region as Region exposing (LocatedSector)
import Croq.Data.Route as Route
import Croq.Data.Sector as Sector
import Croq.Data.Types exposing (..)
import Croq.Pages.SectorPageCommon exposing (httpDataRequest, sectorGet, viewAccess)
import Croq.Routes as Routes
import Croq.Ui as Ui
import Croq.Ui.BoulderMap as Map
import Croq.Ui.Color as Color
import Croq.Ui.Histogram as Histogram
import Croq.Ui.Tab as Tab exposing (Msg(..))
import Croq.Util exposing (maybeCompare, maybeShow)
import Grades.Climbing as Climbing
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy)
import Http
import List.Extra as List
import Markdown
import Maybe.Extra as Maybe
import Table exposing (defaultCustomizations)


type alias Model =
    { id : SectorId
    , data : LoadingHttp LocatedSector
    , tab : Tab.Model
    , map : Map.Model
    , table : Table.State
    , histogram : Histogram.Model
    }


type Msg
    = OnDataReceived (Result Http.Error LocatedSector)
    | OnTabMsg Tab.Msg
    | OnMapMsg Map.Msg
    | OnHistogramMsg Histogram.Msg
    | OnTableUpdate Table.State


entry : Cfg.Model -> SectorId -> ( Model, Cmd Msg )
entry cfg id =
    ( { id = id
      , data = Loading.Pending
      , tab = Tab.init tabConfig
      , map = Map.init
      , table = Table.initialSort "Grau"
      , histogram = Histogram.init
      }
    , httpDataRequest OnDataReceived cfg id
    )


update : Msg -> Cfg.Model -> Model -> ( Model, Cmd Msg )
update msg _ m =
    let
        return model =
            ( model, Cmd.none )
    in
    case msg of
        OnDataReceived data ->
            return { m | data = Loading.fromResult data }

        OnTabMsg msg_ ->
            return { m | tab = Tab.update msg_ m.tab }

        OnMapMsg msg_ ->
            return { m | map = Map.update msg_ m.map }

        OnHistogramMsg msg_ ->
            return { m | histogram = Histogram.update msg_ m.histogram }

        OnTableUpdate state ->
            return { m | table = state }


view : Cfg.Model -> Model -> Html Msg
view _ m =
    Ui.appShell <|
        Ui.container
            [ Ui.breadcrumbs (Region.locatedSectorBreadcrumbs m)
            , Ui.title <| sectorGet .name "Carregando setor..." m
            , Ui.tags <| sectorGet (.tags >> List.map Sector.renderTag) [] m
            , Tab.view tabConfig m.tab m
            ]


viewRoutes : Model -> Html Msg
viewRoutes m =
    Ui.viewLoading m.data <|
        \{ sector } ->
            let
                summary route =
                    let
                        grade =
                            route.grade
                                |> Maybe.map Climbing.show
                                |> Maybe.withDefault "∅"
                    in
                    route.name ++ " (" ++ grade ++ ")"
            in
            div []
                [ Html.map OnMapMsg (Map.view m.map)
                , Ui.title sector.name
                , Ui.cardList a Color.Primary <|
                    (sector.routes
                        |> List.map
                            (\route ->
                                ( summary route, [ href (Routes.routeUrl route.id) ] )
                            )
                    )
                ]


viewInfo : Model -> Html Msg
viewInfo m =
    Ui.viewLoading m.data <|
        \{ sector } ->
            let
                histData =
                    sector.routes
                        |> List.map (.grade >> Maybe.map Climbing.simplify)
                        |> List.sortWith (maybeCompare Climbing.compare)
                        |> List.group
                        |> List.map (\( x, y ) -> ( x, 1 + List.length y ))
                        |> List.sortWith (\( x, _ ) ( y, _ ) -> maybeCompare Climbing.compare x y)
                        |> List.map (\( x, y ) -> ( Maybe.map Climbing.show x |> Maybe.withDefault "∅", toFloat y ))
            in
            Ui.sections
                [ ( "Descrição", [ lazy (Markdown.toHtml []) sector.description ] )
                , ( "Distribuição de graus", [ Html.map OnHistogramMsg (Histogram.view m.histogram histData) ] )
                , ( "Lista de problemas", [ Table.view tableConfig m.table sector.routes ] )
                ]


tabConfig : Tab.Config Model Msg
tabConfig =
    Tab.Config
        OnTabMsg
        [ ( "Map", viewRoutes )
        , ( "Sobre", viewInfo )
        , ( "Acesso", viewAccess )
        ]


tableConfig : Table.Config Route.Route Msg
tableConfig =
    Table.customConfig
        { toId = .name
        , toMsg = OnTableUpdate
        , columns =
            [ Table.stringColumn "Name" .name
            , Table.customColumn
                { name = "Grau"
                , viewData = .grade >> maybeShow Climbing.show
                , sorter =
                    Table.increasingOrDecreasingBy
                        (.grade
                            >> Maybe.map Climbing.toLinearScale
                            >> Maybe.withDefault 0
                        )
                }
            ]
        , customizations = { defaultCustomizations | tableAttrs = [ class "table w-full" ] }
        }
