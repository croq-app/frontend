module TopoEdit.View exposing (..)

import BoundingBox2d as BB
import Dict
import Geometry.Svg as GS
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Point2d as Pt
import Svg as S
import Svg.Attributes as SA
import TopoEdit.Figure exposing (..)
import TopoEdit.GeoUtils exposing (..)
import TopoEdit.Types exposing (..)
import TopoEdit.ViewEditBar
import TopoEdit.ViewFigure exposing (viewFigure)
import Vector2d as Vec


editbar : Model -> Html Msg
editbar =
    TopoEdit.ViewEditBar.editbar


legend : Model -> Html Msg
legend m =
    ol [] <|
        (m.figures
            |> Dict.values
            |> filterMarks (\_ -> True)
            |> List.sortBy .number
            |> List.map (\{ label, number } -> li [] [ text (String.fromInt number ++ " " ++ label) ])
        )


toolbar : Model -> Html Msg
toolbar m =
    div []
        [ button [ class "btn", onClick (addMarkMsg m) ] [ text "+ mark" ] ]


scene : Model -> Html Msg
scene m =
    S.svg
        [ SA.width "100%"
        , SA.viewBox (m.bbox |> viewBox)
        , onDoubleClick( OnSelectFigure -1 -1)
        ]
        (m.figures
            |> Dict.toList
            |> List.map (viewFigure m)
            |> List.concat
        )


centeringAt : Pt -> S.Svg msg -> S.Svg msg
centeringAt pt_ =
    GS.translateBy (Vec.from pt_ Pt.origin)


addMarkMsg : Model -> Msg
addMarkMsg m =
    let
        label =
            m.figures
                |> Dict.values
                |> filterMarks (\_ -> True)
                |> List.map .number
                |> List.maximum
                |> Maybe.withDefault 0
                |> (+) 1

        mark =
            Mark { number = label, data = BB.centerPoint m.bbox, label = "Sem nome" }
    in
    OnPushFigure mark
