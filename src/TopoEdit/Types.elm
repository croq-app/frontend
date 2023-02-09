module TopoEdit.Types exposing (..)

import Dict exposing (Dict)
import Draggable
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import TopoEdit.Figure exposing (..)
import TopoEdit.GeoUtils exposing (..)


type alias Ref =
    Int


type alias Model =
    { figures : Dict Ref Figure
    , index : Int
    , selected : Ref
    , subSelected : Ref
    , drag : Draggable.State ( Ref, Ref )
    , scale : Float
    , bbox : BB
    }


type Msg
    = OnSelectFigure Ref Ref
    | OnDragBy Draggable.Delta
    | OnDragMsg (Draggable.Msg ( Ref, Ref ))
    | OnPushFigure Figure
    | OnUpdateFigure Ref Figure
    | OnInsertPointsAt Ref (List ( Int, Pt ))
    | OnRemovePointsAt Ref (List Int)
    | OnUpdateLabel String
    | OnDeleteFigure Ref
