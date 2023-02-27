module Croq.Pages.BoulderSectorPage exposing (Model, Msg, entry, subscriptions, update, view)

import Croq.Config as Cfg
import Croq.Data.BoulderFormation as BoulderFormation exposing (BoulderFormation)
import Croq.Data.BoulderProblem exposing (showWithGrade)
import Croq.Data.Id exposing (..)
import Croq.Data.Loading as Loading exposing (LoadingHttp)
import Croq.Data.Region as Region exposing (SectorCur)
import Croq.Data.Sector as Sector
import Croq.Data.Types exposing (..)
import Croq.Pages.SectorPageCommon exposing (httpDataRequest, sectorGet, viewAccess)
import Croq.Routes as Routes
import Croq.Ui as Ui
import Croq.Ui.BoulderMap as Map
import Croq.Ui.Histogram as Histogram
import Croq.Util exposing (maybeCompare, maybeCompareLast, maybeShow)
import Daisy.Accordion as Accordion
import Daisy.Elements as Ui
import Daisy.Tab as Tab exposing (Msg(..))
import Grades.Bouldering as Bouldering
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
    , data : LoadingHttp SectorCur
    , selectedFormation : Maybe ElemId
    , accordion : Accordion.State
    , tab : Tab.Model
    , map : Map.Model
    , table : Table.State
    , histogram : Histogram.Model
    }


type Msg
    = OnDataReceived (Result Http.Error SectorCur)
    | OnTabMsg Tab.Msg
    | OnMapMsg Map.Msg
    | OnHistogramMsg Histogram.Msg
    | OnAccordionMsg Accordion.State
    | OnTableUpdate Table.State
    | OnBlockSelect ElemId


entry : Cfg.Model -> SectorId -> ( Model, Cmd Msg )
entry cfg id =
    ( { id = id
      , data = Loading.Pending
      , selectedFormation = Nothing
      , accordion = Accordion.init
      , tab = Tab.init (tabConfig cfg)
      , map = Map.init
      , table = Table.initialSort "Grau"
      , histogram = Histogram.init
      }
    , httpDataRequest OnDataReceived cfg id
    )


update : Cfg.Model -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg_ m =
    let
        return model =
            ( model, Cmd.none )
    in
    case msg_ of
        OnDataReceived data ->
            return { m | data = Loading.fromResult data }

        OnTabMsg msg ->
            return { m | tab = Tab.update msg m.tab }

        OnMapMsg msg ->
            let
                ( map, cmd ) =
                    Map.update msg m.map
            in
            ( { m | map = map }, Cmd.map OnMapMsg cmd )

        OnHistogramMsg msg ->
            return { m | histogram = Histogram.update msg m.histogram }

        OnTableUpdate state ->
            return { m | table = state }

        OnBlockSelect id ->
            return { m | selectedFormation = Just id }

        OnAccordionMsg state ->
            return { m | accordion = state }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Cfg.Model -> Model -> Html Msg
view cfg m =
    Ui.container
        [ Ui.breadcrumbs (Region.sectorBreadcrumbs m)
        , Ui.title <| sectorGet .name "Carregando setor..." m
        , Ui.tags <| sectorGet (.tags >> List.map Sector.renderTag) [] m
        , Tab.view (tabConfig cfg) m.tab m
        ]


viewBoulders : Cfg.Model -> Model -> Html Msg
viewBoulders cfg m =
    Ui.viewLoading m.data <|
        \{ sector } ->
            div []
                [ Html.map OnMapMsg (Map.view m.map)
                , Accordion.view (accordionConfig cfg) m.accordion sector.boulders
                ]


viewInfo : Cfg.Model -> Model -> Html Msg
viewInfo cfg m =
    Ui.viewLoading m.data <|
        \{ sector } ->
            let
                problems =
                    infos m
                        |> List.map (Tuple.pair cfg)

                histData =
                    problems
                        |> List.map (Tuple.second >> .problem >> .grade >> Maybe.map Bouldering.simplify)
                        |> List.sortWith (maybeCompare Bouldering.compare)
                        |> List.group
                        |> List.map (\( x, y ) -> ( x, 1 + List.length y ))
                        |> List.sortWith (\( x, _ ) ( y, _ ) -> maybeCompare Bouldering.compare x y)
                        |> List.map (\( x, y ) -> ( Maybe.map (Cfg.showBoulderingGrade cfg) x |> Maybe.withDefault "∅", toFloat y ))
            in
            Ui.sections []
                [ ( "Descrição", [ lazy (Markdown.toHtml []) sector.description ] )
                , ( "Distribuição de graus", [ Html.map OnHistogramMsg (Histogram.view m.histogram histData) ] )
                , ( "Lista de problemas", [ Table.view tableConfig m.table problems ] )
                ]


viewFormation : Cfg.Model -> BoulderFormation -> Html Msg
viewFormation cfg boulder =
    let
        cmp x y =
            maybeCompareLast Bouldering.compare x.grade y.grade

        problems =
            List.sortWith cmp boulder.problems
    in
    div []
        [ Ui.list (showWithGrade cfg >> text) [] problems
        , Ui.actionBtn
            [ href (Routes.boulderFormationUrl boulder.id) ]
            [ text "Ir para o bloco" ]
        ]


infos : Model -> List BoulderFormation.BoulderInfo
infos m =
    case m.selectedFormation of
        Nothing ->
            sectorGet .boulders [] m
                |> List.map BoulderFormation.infos
                |> List.concat

        Just id ->
            sectorGet .boulders [] m
                |> elemFromId id
                |> Maybe.map BoulderFormation.infos
                |> Maybe.withDefault []


tabConfig : Cfg.Model -> Tab.Config Model Msg
tabConfig cfg =
    Tab.Config
        OnTabMsg
        [ ( "Blocos", viewBoulders cfg )
        , ( "Sobre", viewInfo cfg )
        , ( "Acesso", viewAccess cfg )
        ]


tableConfig : Table.Config ( Cfg.Model, BoulderFormation.BoulderInfo ) Msg
tableConfig =
    Table.customConfig
        { toId = Tuple.second >> .problem >> .name
        , toMsg = OnTableUpdate
        , columns =
            [ Table.stringColumn "Name" (Tuple.second >> .problem >> .name)
            , Table.stringColumn "Bloco" (Tuple.second >> .formation >> .name)
            , Table.customColumn
                { name = "Grau"
                , viewData = \( cfg, { problem } ) -> maybeShow (Cfg.showBoulderingGrade cfg) problem.grade
                , sorter =
                    Table.increasingOrDecreasingBy
                        (Tuple.second
                            >> .problem
                            >> .grade
                            >> Maybe.map Bouldering.toLinearScale
                            >> Maybe.withDefault 0
                        )
                }
            ]
        , customizations = { defaultCustomizations | tableAttrs = [ class "table w-full" ] }
        }


accordionConfig : Cfg.Model -> Accordion.Config BoulderFormation.BoulderFormation Msg
accordionConfig cfg =
    Accordion.config OnAccordionMsg .name (viewFormation cfg)
