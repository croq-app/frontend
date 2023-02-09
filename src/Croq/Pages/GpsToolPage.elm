port module Croq.Pages.GpsToolPage exposing (..)

import Chart as C
import Chart.Attributes as CA
import Croq.Ui as Ui
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode exposing (Value)
import LatLng exposing (toTupleDegrees)
import PortFunnel exposing (FunnelSpec)
import PortFunnel.Geolocation as Geo
import Round
import Time


type alias Model =
    { locations : LatLng.Polyline
    , error : Maybe String
    , state : Geo.State
    , time : Float
    , start : Float
    }


type Msg
    = OnRequestLocation
    | OnTick Float
    | OnDataReceived Value


type alias Funnel =
    FunnelSpec Geo.State Geo.State Geo.Message Geo.Response Model Msg


port cmdPort : Value -> Cmd msg


port subPort : (Value -> msg) -> Sub msg


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ subPort OnDataReceived
        , Time.every 10 (Time.posixToMillis >> toFloat >> (*) 0.001 >> OnTick)
        ]


entry : ( Model, Cmd.Cmd Msg )
entry =
    ( { locations = []
      , state = Geo.initialState
      , error = Nothing
      , time = 0
      , start = -1
      }
    , Cmd.batch [ send Geo.watchChanges ]
    )


funnels : Dict String Funnel
funnels =
    let
        handler : Geo.Response -> b -> Model -> ( Model, Cmd Msg )
        handler response _ m =
            case response of
                Geo.LocationResponse loc ->
                    ( { m
                        | locations =
                            let
                                pt =
                                    LatLng.fromLatLngDegrees loc.latitude loc.longitude
                            in
                            if List.head m.locations == Just pt then
                                m.locations

                            else
                                pt :: m.locations
                      }
                    , Cmd.none
                    )

                Geo.ErrorResponse error ->
                    ( { m | error = Just (Geo.errorToString error) }, Cmd.none )

                _ ->
                    ( m, Cmd.none )
    in
    Dict.fromList
        [ ( Geo.moduleName
          , FunnelSpec
                { get = identity, set = \state _ -> state }
                Geo.moduleDesc
                Geo.commander
                handler
          )
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        OnRequestLocation ->
            ( m
            , send
                (Geo.nowWith
                    { enableHighAccuracy = True
                    , timeout = Nothing
                    , maximumAge = Nothing
                    }
                )
            )

        OnDataReceived value ->
            case
                PortFunnel.processValue funnels (PortFunnel.appProcess cmdPort) value m.state m
            of
                Err error ->
                    ( { m | error = Just error }, Cmd.none )

                Ok res ->
                    res

        OnTick t ->
            update OnRequestLocation <|
                if m.start == -1 then
                    { m | start = t, time = 0 }

                else
                    { m | time = t - m.start }


send : Geo.Message -> Cmd Msg
send message =
    Geo.send cmdPort message


view : Model -> Html Msg
view m =
    Ui.appShell <|
        Ui.container <|
            [ p []
                [ button
                    [ onClick OnRequestLocation, class "btn" ]
                    [ text "Get location" ]
                , text (iff (Geo.isLoaded m.state) "loaded" "unloaded")
                ]
            , div [ class "font-mono" ] [ text ("t: " ++ Round.round 2 m.time ++ " n: " ++ String.fromInt (List.length m.locations)) ]
            , C.chart
                [ CA.height 300
                , CA.width 300
                ]
                [ C.xLabels [ CA.withGrid ]
                , C.yLabels [ CA.withGrid ]
                , C.series Tuple.first
                    [ C.scatter Tuple.second []
                    ]
                    (m.locations |> List.map toTupleDegrees)
                ]
            ]


svgPath : List ( Float, Float ) -> String
svgPath lst =
    let
        pt s ( x, y ) =
            s ++ String.fromFloat x ++ "," ++ String.fromFloat y
    in
    case lst of
        [] ->
            "M 0.0,0.0"

        x :: rest ->
            pt "M" x
                ++ " "
                ++ (rest
                        |> List.map (pt "L")
                        |> String.join " "
                   )


iff : Bool -> c -> c -> c
iff a b c =
    if a then
        b

    else
        c
