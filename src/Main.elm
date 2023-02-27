module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Croq.Config as Cfg
import Croq.Data.Types exposing (..)
import Croq.Pages.BoulderFormationPage as BoulderFormation
import Croq.Pages.BoulderProblemPage as BoulderProblem
import Croq.Pages.BoulderSectorPage as BoulderSector
import Croq.Pages.GpsToolPage as GpsTool
import Croq.Pages.GradeToolPage as GradeTool
import Croq.Pages.HomePage as Home
import Croq.Pages.ParkingPage as Parking
import Croq.Pages.RegionPage as Region
import Croq.Pages.RoutePage as Route
import Croq.Pages.RouteSectorPage as RouteSector
import Croq.Pages.SimplePage as Playground
import Croq.Pages.TopoToolPage as TopoTool
import Croq.Routes exposing (Route(..), parseUrl)
import Croq.Ui
import Html exposing (Html, div, text)
import Url exposing (Url)


type Page
    = HomePage Home.Model
    | ParkingPage Parking.Model
    | RegionPage Region.Model
    | BoulderSectorPage BoulderSector.Model
    | BoulderFormationPage BoulderFormation.Model
    | BoulderProblemPage BoulderProblem.Model
    | RouteSectorPage RouteSector.Model
    | RoutePage Route.Model
    | GpsToolPage GpsTool.Model
    | GradeToolPage GradeTool.Model
    | TopoToolPage TopoTool.Model
    | PlaygroundPage Playground.Model
    | ErrorPage String


type alias Model =
    { page : Page
    , cfg : Cfg.Model
    }


type Msg
    = OnPushUrl String
    | OnUrlChange Url
    | OnUrlRequest Browser.UrlRequest
    | OnConfigMsg Cfg.Msg
    | OnHomeMsg Home.Msg
    | OnRegionMsg Region.Msg
    | OnBoulderSectorMsg BoulderSector.Msg
    | OnBoulderFormationMsg BoulderFormation.Msg
    | OnBoulderProblemMsg BoulderProblem.Msg
    | OnRouteSectorMsg RouteSector.Msg
    | OnRouteMsg Route.Msg
    | OnParkingMsg Parking.Msg
    | OnGpsToolMsg GpsTool.Msg
    | OnGradeToolMsg GradeTool.Msg
    | OnTopoToolMsg TopoTool.Msg
    | OnPlaygroundMsg Playground.Msg
    | NoOp


pageFromRoute : Route -> Cfg.Model -> ( Model, Cmd Msg )
pageFromRoute r cfg =
    let
        page wrap msg ( m, cmd ) =
            ( { page = wrap m, cfg = cfg }, Cmd.map msg cmd )
    in
    case r of
        Home ->
            page HomePage OnHomeMsg (Home.entry cfg)

        Region id ->
            page RegionPage OnRegionMsg (Region.entry cfg id)

        BoulderSector req ->
            page BoulderSectorPage OnBoulderSectorMsg (BoulderSector.entry cfg req)

        BoulderFormation req ->
            page BoulderFormationPage OnBoulderFormationMsg (BoulderFormation.entry cfg req)

        BoulderProblem req ->
            page BoulderProblemPage OnBoulderProblemMsg (BoulderProblem.entry cfg req)

        RouteSector req ->
            page RouteSectorPage OnRouteSectorMsg (RouteSector.entry cfg req)

        Route req ->
            page RoutePage OnRouteMsg (Route.entry cfg req)

        Parking req ->
            page ParkingPage OnParkingMsg (Parking.entry req)

        GpsTool ->
            page GpsToolPage OnGpsToolMsg GpsTool.entry

        GradeTool ->
            page GradeToolPage OnGradeToolMsg ( GradeTool.init, Cmd.none )

        TopoTool ->
            page TopoToolPage OnTopoToolMsg ( TopoTool.init, Cmd.none )

        Playground ->
            page PlaygroundPage OnPlaygroundMsg ( Playground.init, Cmd.none )

        Error msg ->
            page ErrorPage (\_ -> NoOp) ( msg, Cmd.none )


init : Cfg.Model -> Model
init cfg =
    Tuple.first <| pageFromRoute Home cfg


view : Model -> Html Msg
view { page, cfg } =
    let
        shell =
            Croq.Ui.appShell cfg OnConfigMsg

        render viewf msg m =
            shell msg (viewf cfg m)
    in
    case page of
        HomePage m ->
            render Home.view OnHomeMsg m

        RegionPage m ->
            render Region.view OnRegionMsg m

        BoulderSectorPage m ->
            render BoulderSector.view OnBoulderSectorMsg m

        BoulderFormationPage m ->
            render BoulderFormation.view OnBoulderFormationMsg m

        BoulderProblemPage m ->
            render BoulderProblem.view OnBoulderProblemMsg m

        RouteSectorPage m ->
            render RouteSector.view OnRouteSectorMsg m

        RoutePage m ->
            render Route.view OnRouteMsg m

        ParkingPage m ->
            render Parking.view OnParkingMsg m

        GpsToolPage m ->
            render GpsTool.view OnGpsToolMsg m

        GradeToolPage m ->
            render GradeTool.view OnGradeToolMsg m

        TopoToolPage m ->
            render TopoTool.view OnTopoToolMsg m

        PlaygroundPage m ->
            render Playground.view OnPlaygroundMsg m

        ErrorPage st ->
            render (\_ _ -> div [] [ text ("ERROR :" ++ st) ]) identity ()


update : Msg -> Model -> ( Model, Cmd Msg )
update msg_ m_ =
    let
        cfg =
            m_.cfg

        page m1 m2 ( p, cmd ) =
            ( { m_ | page = m1 p }, Cmd.map m2 cmd )
    in
    case ( msg_, m_.page ) of
        -- Routing and generic navigation
        ( OnPushUrl st, _ ) ->
            ( m_, Cfg.pushUrl st cfg )

        ( OnUrlRequest (Browser.Internal url), _ ) ->
            update (OnPushUrl (Url.toString url)) m_

        ( OnUrlRequest (Browser.External url), _ ) ->
            ( m_, Nav.load url )

        ( OnUrlChange url, _ ) ->
            pageFromRoute (parseUrl url) cfg

        -- Redirect to appropriate sub-model
        ( OnHomeMsg msg, HomePage m ) ->
            page HomePage OnHomeMsg (Home.update msg m)

        ( OnRegionMsg msg, RegionPage m ) ->
            page RegionPage OnRegionMsg (Region.update msg m)

        ( OnBoulderSectorMsg msg, BoulderSectorPage m ) ->
            page BoulderSectorPage OnBoulderSectorMsg (BoulderSector.update cfg msg m)

        ( OnBoulderFormationMsg msg, BoulderFormationPage m ) ->
            page BoulderFormationPage OnBoulderFormationMsg (BoulderFormation.update cfg msg m)

        ( OnBoulderProblemMsg msg, BoulderProblemPage m ) ->
            page BoulderProblemPage OnBoulderProblemMsg ( BoulderProblem.update msg m, Cmd.none )

        ( OnRouteSectorMsg msg, RouteSectorPage m ) ->
            page RouteSectorPage OnRouteSectorMsg (RouteSector.update cfg msg m)

        ( OnRouteMsg msg, RoutePage m ) ->
            page RoutePage OnRouteMsg ( Route.update msg m, Cmd.none )

        ( OnGpsToolMsg msg, GpsToolPage m ) ->
            page GpsToolPage OnGpsToolMsg (GpsTool.update msg m)

        ( OnGradeToolMsg msg, GradeToolPage m ) ->
            page GradeToolPage OnGradeToolMsg ( GradeTool.update msg m, Cmd.none )

        ( OnTopoToolMsg msg, TopoToolPage m ) ->
            page TopoToolPage OnTopoToolMsg (TopoTool.update msg m)

        -- Internal state and other global tasks
        ( OnConfigMsg msg, _ ) ->
            let
                ( cfg_, cmd ) =
                    Cfg.update msg cfg
            in
            ( { m_ | cfg = cfg_ }, Cmd.map OnConfigMsg cmd )

        ( NoOp, _ ) ->
            ( m_, Cmd.none )

        _ ->
            ( m_, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model.page of
            HomePage m ->
                Sub.map OnHomeMsg (Home.subscriptions m)

            RegionPage m ->
                Sub.map OnRegionMsg (Region.subscriptions m)

            BoulderSectorPage m ->
                Sub.map OnBoulderSectorMsg (BoulderSector.subscriptions m)

            BoulderFormationPage m ->
                Sub.map OnBoulderFormationMsg (BoulderFormation.subscriptions m)

            BoulderProblemPage m ->
                Sub.map OnBoulderProblemMsg (BoulderProblem.subscriptions m)

            RouteSectorPage m ->
                Sub.map OnRouteSectorMsg (RouteSector.subscriptions m)

            RoutePage m ->
                Sub.map OnRouteMsg (Route.subscriptions m)

            ParkingPage m ->
                Sub.map OnParkingMsg (Parking.subscriptions m)

            GpsToolPage m ->
                Sub.map OnGpsToolMsg (GpsTool.subscriptions m)

            GradeToolPage m ->
                Sub.map OnGradeToolMsg (GradeTool.subscriptions m)

            TopoToolPage m ->
                Sub.map OnTopoToolMsg (TopoTool.subscriptions m)

            PlaygroundPage m ->
                Sub.map OnPlaygroundMsg (Playground.subscriptions m)

            _ ->
                Sub.none
        ]


main : Program String Model Msg
main =
    let
        initFn hostname url key =
            update (OnUrlChange url) (init (Cfg.init hostname key))
    in
    Browser.application
        { init = initFn
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        , view = \m -> Browser.Document "Croq.app" [ view m ]
        }
