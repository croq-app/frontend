module TopoEdit.GeoUtils exposing (..)

import BoundingBox2d as BB
import Circle2d as Circ
import Length as Len exposing (Meters)
import Point2d as Pt exposing (Point2d)
import Polyline2d as Line exposing (Polyline2d)
import Quantity as Q
import Vector2d as Vec


type alias Pt =
    Pt.Point2d Meters ()


type alias Vec =
    Vec.Vector2d Meters ()


type alias Line =
    Line.Polyline2d Meters ()


type alias Circ =
    Circ.Circle2d Meters ()


type alias BB =
    BB.BoundingBox2d Meters ()


vec : Float -> Float -> Vec
vec x y =
    Vec.fromMeters { x = x, y = y }


pt : Float -> Float -> Pt
pt x y =
    Pt.fromMeters { x = x, y = y }


line : List ( Float, Float ) -> Line
line =
    List.map (\( x, y ) -> pt x y) >> Line.fromVertices


viewBox : BB -> String
viewBox bb =
    let
        { minX, minY, maxX, maxY } =
            BB.extrema bb
    in
    [ minX, minY, Q.difference maxX minX, Q.difference maxY minY ]
        |> List.map (Len.inMeters >> round >> String.fromInt)
        |> String.join " "


iff : Bool -> c -> c -> c
iff a b c =
    if a then
        b

    else
        c



-------------------------------------------------------------------------------
--- Polyline functions
-------------------------------------------------------------------------------


lineIndexedMapVertices : (Int -> Point2d units coordinates -> Point2d units coordinates) -> Polyline2d units coordinates -> Polyline2d units coordinates
lineIndexedMapVertices function =
    Line.vertices >> List.indexedMap function >> Line.fromVertices
