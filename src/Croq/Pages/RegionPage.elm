module Croq.Pages.RegionPage exposing (Model, Msg, entry, subscriptions, update, view)

import Croq.Api as Api
import Croq.Config as Cfg
import Croq.Data.Id exposing (..)
import Croq.Data.Loading as Loading exposing (LoadingHttp)
import Croq.Data.Region as Region exposing (Region)
import Croq.Data.Types exposing (..)
import Croq.Routes as Routes
import Croq.Ui as Ui
import Croq.Ui.Color as Color
import Croq.Ui.SectorMap as Map
import Daisy.Elements as Ui
import Daisy.Tab as Tab
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import List.Extra as List


type alias Model =
    { id : RegionId
    , data : LoadingHttp Region
    , tab : Tab.Model
    , map : Map.Model
    , selectedSector : Int
    , selectedAttraction : Int
    , showAttraction : Bool
    }


type Msg
    = OnDataReceived (Result Http.Error Region)
    | OnChangeSelectedSector Int
    | OnChangeSelectedAttraction Int
    | OnShowAttraction Int
    | OnTabMsg Tab.Msg
    | OnMapMsg Map.Msg


entry : Cfg.Model -> RegionId -> ( Model, Cmd Msg )
entry cfg id =
    let
        m =
            { id = id
            , data = Loading.Pending
            , tab = Tab.init tabConfig
            , map = Map.init
            , selectedSector = -1
            , selectedAttraction = -1
            , showAttraction = False
            }
    in
    ( m, httpDataRequest cfg id )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg_ m =
    let
        return m_ =
            ( m_, Cmd.none )
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

        OnChangeSelectedSector i ->
            return { m | selectedSector = i }

        OnChangeSelectedAttraction i ->
            return { m | selectedAttraction = i }

        OnShowAttraction i ->
            return { m | showAttraction = i >= 0, selectedAttraction = i }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Cfg.Model -> Model -> Html Msg
view _ m =
    div []
        [ Ui.container
            [ Ui.breadcrumbs (Region.breadcrumbs m)
            , Ui.title "Mapa dos setores"
            ]
        , div [ class "max-w-lg mx-auto" ] [ Html.map OnMapMsg <| Map.view m.map ]
        , Ui.container [ Tab.view tabConfig m.tab m ]
        ]


viewSectors : Model -> Html Msg
viewSectors m =
    let
        card sector =
            let
                url =
                    Routes.sectorUrl sector.kind sector.id
            in
            ( sector.name, [ href url ] )
    in
    Ui.viewLoading m.data <|
        \{ sectors } ->
            if sectors == [] then
                div [ class "card m-4 p-4 bg-focus" ] [ text "Nenhum setor cadastrado!" ]

            else
                Ui.cardList a Color.Primary (List.map card sectors)


viewAttractions : Model -> Html Msg
viewAttractions m =
    Ui.viewLoading m.data <|
        \{ attractions } ->
            if attractions == [] then
                div [ class "card m-4 p-4 bg-error" ] [ text "Nenhuma atração cadastrada!" ]

            else if m.showAttraction then
                div []
                    [ button [ onClick (OnShowAttraction -1) ] [ text "< back" ]
                    , List.getAt m.selectedAttraction attractions
                        |> Maybe.map (viewRichText << .description)
                        |> Maybe.withDefault (text "internal error")
                    ]

            else
                Ui.cardList button
                    Color.Secondary
                    (List.indexedMap
                        (\i a -> ( a.name, [ onClick (OnShowAttraction i) ] ))
                        attractions
                    )


tabConfig : Tab.Config Model Msg
tabConfig =
    Tab.config OnTabMsg [ ( "Setores", viewSectors ), ( "Atrações", viewAttractions ) ]


httpDataRequest : Cfg.Model -> RegionId -> Cmd Msg
httpDataRequest cfg id =
    Http.get
        { url = Api.regionUrl cfg id
        , expect = Http.expectJson OnDataReceived Region.decoder
        }
