module TopoEdit.Figure exposing (..)

import Basics.Extra exposing (flip)
import BoundingBox2d as BB
import List.Extra as LE
import Maybe.Extra
import Point2d as Pt
import Polyline2d as Line
import TopoEdit.GeoUtils exposing (..)


type Figure
    = Line LineData
    | Mark MarkData


type alias MarkData =
    { number : Int
    , data : Pt
    , label : String
    }


type alias LineData =
    { data : Line }



-------------------------------------------------------------------------------
--- Properties
-------------------------------------------------------------------------------


boundingBox : Figure -> BB
boundingBox fig =
    case fig of
        Mark { data } ->
            BB.singleton data

        _ ->
            BB.singleton (Pt.meters 0 0)


translateBy : Vec -> Figure -> Figure
translateBy displacement fig =
    case fig of
        Mark m ->
            Mark { m | data = Pt.translateBy displacement m.data }

        Line m ->
            Line { m | data = Line.translateBy displacement m.data }



-------------------------------------------------------------------------------
--- Querying and transforming groups of figures
-------------------------------------------------------------------------------


moveInternalPointBy : Int -> Vec -> Figure -> Figure
moveInternalPointBy idx displacement fig =
    case fig of
        Line m ->
            Line
                { m
                    | data =
                        m.data
                            |> Line.vertices
                            |> LE.updateAt idx (Pt.translateBy displacement)
                            |> Line.fromVertices
                }

        _ ->
            fig


insertInternalPointAt : Int -> Pt -> Figure -> Figure
insertInternalPointAt idx pt_ fig =
    case fig of
        Line m ->
            case LE.splitAt idx (Line.vertices m.data) of
                ( [], tail ) ->
                    Line { m | data = Line.fromVertices (pt_ :: tail) }

                ( left, right ) ->
                    Line { m | data = Line.fromVertices (left ++ (pt_ :: right)) }

        _ ->
            fig


removeInternalPoints : List Int -> Figure -> Figure
removeInternalPoints idxs fig =
    case fig of
        Line m ->
            Line
                { m | data = m.data |> Line.vertices |> LE.removeIfIndex (isIn idxs) |> Line.fromVertices }

        _ ->
            fig


isIn =
    flip List.member



-------------------------------------------------------------------------------
--- Querying and transforming groups of figures
-------------------------------------------------------------------------------


filterMarks : (MarkData -> Bool) -> List Figure -> List MarkData
filterMarks pred =
    Maybe.Extra.values
        << List.map
            (\fig ->
                case fig of
                    Mark data ->
                        if pred data then
                            Just data

                        else
                            Nothing

                    _ ->
                        Nothing
            )


filterLines : (LineData -> Bool) -> List Figure -> List LineData
filterLines pred =
    Maybe.Extra.values
        << List.map
            (\fig ->
                case fig of
                    Line data ->
                        if pred data then
                            Just data

                        else
                            Nothing

                    _ ->
                        Nothing
            )


mapMarks : (MarkData -> Figure) -> List Figure -> List Figure
mapMarks f =
    List.map
        (\fig ->
            case fig of
                Mark data ->
                    f data

                _ ->
                    fig
        )


mapLines : (LineData -> Figure) -> List Figure -> List Figure
mapLines f =
    List.map
        (\fig ->
            case fig of
                Line data ->
                    f data

                _ ->
                    fig
        )
