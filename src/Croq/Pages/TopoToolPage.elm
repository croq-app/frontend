module Croq.Pages.TopoToolPage exposing (Model, Msg, init, subscriptions, update, view)

{-| Example bare bones page
-}

import BoundingBox2d as BB
import Croq.Config as Cfg
import Croq.Ui as Ui
import Croq.Ui as Ui
import Dict exposing (Dict)
import Draggable
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Length exposing (meters)
import Point2d as Pt
import TopoEdit.Figure as Figure exposing (Figure(..))
import TopoEdit.GeoUtils exposing (..)
import TopoEdit.Types exposing (..)
import TopoEdit.View
import Vector2d as Vec


type alias Msg =
    TopoEdit.Types.Msg


type alias Model =
    TopoEdit.Types.Model


init : Model
init =
    { figures =
        Dict.fromList
            [ ( 0, Mark { number = 1, data = Pt.meters 0 0, label = "Route 1" } )
            , ( 1, Mark { number = 2, data = Pt.meters 0 5, label = "Route 2" } )
            , ( 2, Mark { number = 3, data = Pt.meters 2 3, label = "Route 3" } )
            , ( 3, Line { data = line [ ( 1, 5 ), ( 3, 4 ), ( 0, -1 ) ] } )
            ]
    , index = 2
    , selected = 2
    , subSelected = -1
    , scale = 0.045
    , bbox = BB.fromExtrema { minX = meters -10, minY = meters -10, maxX = meters 10, maxY = meters 10 }
    , drag = Draggable.init
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    let
        noCmd m_ =
            ( m_, Cmd.none )

        mapFigures f =
            noCmd { m | figures = f m.figures }

        mapSingle key f =
            mapFigures
                (\dic ->
                    case Dict.get key dic of
                        Just fig ->
                            Dict.insert key (f fig) dic

                        Nothing ->
                            dic
                )
    in
    case msg of
        OnSelectFigure ref sub ->
            noCmd { m | selected = ref, subSelected = sub }

        OnDragBy ( dx, dy ) ->
            let
                displacement =
                    vec dx dy |> Vec.scaleBy m.scale
            in
            noCmd <|
                mapSelectedPoint
                    (if m.subSelected >= 0 then
                        Figure.moveInternalPointBy m.subSelected displacement

                     else
                        Figure.translateBy displacement
                    )
                    m

        OnDragMsg dragMsg ->
            Draggable.update dragConfig dragMsg m

        OnPushFigure figure ->
            mapFigures (addFigure figure)

        OnUpdateFigure ref figure ->
            mapFigures (Dict.insert ref figure)

        OnUpdateLabel label ->
            case Dict.get m.selected m.figures of
                Just (Mark data) ->
                    update (OnUpdateFigure m.selected (Mark { data | label = label })) m

                _ ->
                    noCmd m

        OnDeleteFigure ref ->
            mapFigures (Dict.remove ref)

        OnInsertPointsAt ref pts ->
            mapSingle ref
                (\fig ->
                    let
                        addPt : ( Int, Pt ) -> Figure -> Figure
                        addPt ( idx, pt_ ) acc =
                            Figure.insertInternalPointAt idx pt_ acc
                    in
                    List.sortBy (Tuple.first >> negate) pts
                        |> List.foldl addPt fig
                )

        OnRemovePointsAt ref idxs ->
            mapSingle ref (Figure.removeInternalPoints idxs)


view : Cfg.Model -> Model -> Html Msg
view _ m =
    (Ui.appShell << Ui.container) <|
        [ Ui.title "Editor de croquis"
        , TopoEdit.View.toolbar m
        , TopoEdit.View.legend m
        , TopoEdit.View.scene m
        , TopoEdit.View.editbar m
        ]


subscriptions : Model -> Sub Msg
subscriptions { drag } =
    Draggable.subscriptions OnDragMsg drag


dragConfig : Draggable.Config ( Ref, Ref ) Msg
dragConfig =
    Draggable.basicConfig OnDragBy


addFigure : a -> Dict Int a -> Dict Int a
addFigure fig figures =
    let
        key =
            Dict.keys figures |> List.maximum |> Maybe.withDefault -1 |> (+) 1
    in
    Dict.insert key fig figures


mapSelectedPoint : (Figure -> Figure) -> Model -> Model
mapSelectedPoint f m =
    case Dict.get m.selected m.figures of
        Just fig ->
            { m | figures = Dict.insert m.selected (f fig) m.figures }

        Nothing ->
            m
