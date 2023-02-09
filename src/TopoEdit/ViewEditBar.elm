module TopoEdit.ViewEditBar exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra
import Point2d as Pt
import Polyline2d as Line
import TopoEdit.Figure exposing (..)
import TopoEdit.GeoUtils exposing (..)
import TopoEdit.Types exposing (..)


editbar : Model -> Html Msg
editbar m =
    case Dict.get m.selected m.figures of
        Just (Mark data) ->
            editMark data m.selected

        Just (Line data) ->
            editLine data m.selected m.subSelected

        _ ->
            div [] []


editMark : MarkData -> Int -> Html Msg
editMark { number, data, label } key =
    let
        btn txt by =
            button [ class "btn", onClick <| OnUpdateFigure key (Mark { number = number + by, data = data, label = label }) ] [ text txt ]
    in
    div []
        [ btn "-" -1
        , text "n: "
        , input [ class "input input-bordered", value (String.fromInt number) ]
            []
        , btn "+" 1
        , input [ class "input input-bordered", value label, onInput OnUpdateLabel ] []
        , button [ class "btn" ] [ text "GO" ]
        ]


editLine : LineData -> Int -> Int -> Html Msg
editLine { data } key idx =
    let
        newPoints =
            case List.Extra.splitAt idx (Line.vertices data) of
                ( [], pt0 :: _ ) ->
                    [ ( idx, Pt.translateBy (vec 1 1) pt0 ) ]

                ( _, [ pt0 ] ) ->
                    [ ( idx + 1, Pt.translateBy (vec 1 1) pt0 ) ]

                ( _, pt0 :: _ ) ->
                    [ ( idx, Pt.translateBy (vec -1 -1) pt0 )
                    , ( idx + 1, Pt.translateBy (vec 1 1) pt0 )
                    ]

                _ ->
                    []
    in
    div []
        [ button
            [ class "btn", onClick <| OnInsertPointsAt key newPoints ]
            [ text "+" ]
        , button
            [ class "btn", onClick <| OnRemovePointsAt key [ idx ] ]
            [ text "-" ]
        ]
