module TopoEdit.ViewFigure exposing (..)

import Circle2d as Circ
import Draggable
import Geometry.Svg as GS
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Length as Len exposing (Length, meters)
import Point2d as Pt
import Polyline2d as Line
import Svg as S
import Svg.Attributes as SA
import Svg.Events as SE
import TopoEdit.Figure exposing (..)
import TopoEdit.GeoUtils exposing (..)
import TopoEdit.Types exposing (..)


markRadius : Length
markRadius =
    meters 0.5


pointRadius : Length
pointRadius =
    meters 0.4


viewFigure : Model -> ( Ref, Figure ) -> List (S.Svg Msg)
viewFigure m ( ref, fig ) =
    let
        select =
            iff (ref == m.selected)

        draggable doSelect index node attrs =
            let
                dragAttrs =
                    Draggable.mouseTrigger ( ref, index ) OnDragMsg :: Draggable.touchTriggers ( ref, index ) OnDragMsg
            in
            if doSelect then
                node <| (SE.onMouseDown (OnSelectFigure -1 index) :: attrs ++ dragAttrs)

            else
                node <| SE.onMouseOver (OnSelectFigure ref index) :: attrs
    in
    case fig of
        Mark { number, data } ->
            let
                textXY =
                    data |> Pt.translateBy (vec -0.25 0.25)
            in
            [ draggable (ref == m.selected) -1 S.g [] <|
                [ GS.circle2d
                    [ SA.stroke (select "black" "none"), SA.strokeWidth "0.5", SA.strokeOpacity "0.3" ]
                    (markCircle data)
                , S.text_
                    [ SA.fontSize "0.8"
                    , SA.fontWeight "bold"
                    , xAttr textXY
                    , yAttr textXY
                    , SA.fill "white"
                    , attribute "user-select" "none"
                    ]
                    [ S.text (String.fromInt number) ]
                ]
            ]

        Line { data } ->
            S.g
                []
                [ GS.polyline2d
                    [ SA.fill "none"
                    , SA.stroke "orange"
                    , SA.strokeOpacity (select "0.6" "0.4")
                    , SA.strokeWidth "0.2"
                    ]
                    data
                ]
                :: List.indexedMap
                    (\i pt_ ->
                        draggable (ref == m.selected && i == m.subSelected)
                            i
                            GS.circle2d
                            [ SA.stroke (select (iff (i == m.subSelected) "red" "yellow") "none"), SA.strokeWidth "0.2", SA.strokeOpacity "0.3", SA.fill "yellow" ]
                            (Circ.withRadius pointRadius pt_)
                    )
                    (Line.vertices data)



-- (let
--     vertices =
--         Line.vertices data
--  in
--  if isSelected then
--     vertices
--  else
--     ME.values [ List.head vertices, LE.last vertices ]
-- )


xAttr : Pt -> Attribute msg
xAttr =
    Pt.xCoordinate >> Len.inMeters >> String.fromFloat >> SA.x


yAttr : Pt -> Attribute msg
yAttr =
    Pt.yCoordinate >> Len.inMeters >> String.fromFloat >> SA.y


markCircle : Pt -> Circ
markCircle pt_ =
    Circ.withRadius markRadius pt_
