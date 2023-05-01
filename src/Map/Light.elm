module Map.Light exposing (..)

import Map.World as World
import Point2d

type LightKind
    = Point

type alias Light =
    { kind : LightKind
    , position : World.Point
    }

point : Light -> World.Point
point {position} = position

newPoint : World.Point -> Light
newPoint p = { kind = Point, position = p }

translateBy : World.Vector -> Light -> Light
translateBy vec l =
    { l
    | position = Point2d.translateBy vec l.position
    }
