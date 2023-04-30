module Map.Object exposing (..)

import Dict exposing (Dict)
import Map.World as World
import Rectangle2d
import Polygon2d
import Point2d

type ObjectKind
    = Wall World.Point World.Point

type alias Object =
    { kind : ObjectKind }

polygon : Object -> World.Polygon
polygon {kind} =
    case kind of
        Wall pt1 pt2 -> 
            Rectangle2d.from pt1 pt2 
            |> Rectangle2d.toPolygon


boundingBox : Object -> Maybe World.BoundingBox
boundingBox =
    polygon >> Polygon2d.boundingBox

newWall : World.Point -> World.Point -> Object
newWall p1 p2 =
    { kind = Wall p1 p2 }

translateObjectByVector : World.Vector -> Object -> Object
translateObjectByVector vec obj =
    case obj.kind of
        Wall pt1 pt2 -> 
            { kind = Wall 
                (Point2d.translateBy vec pt1) 
                (Point2d.translateBy vec pt2)
            }
