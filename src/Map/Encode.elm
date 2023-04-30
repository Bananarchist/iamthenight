module Map.Encode exposing (..)
import BoundingBox2d exposing (BoundingBox2d)

import Dict exposing (Dict)
import Json.Encode exposing (..)
import Point2d
import Map.World as World
import Map.Light exposing (Light)
import Map.Room exposing (Room)
import Polygon2d
import Map.Shadow exposing (Shadow)
import Map.Object exposing (ObjectKind(..), Object)
import Map.Level exposing (Level)

encode : Level -> String
encode = 
    level
    >> Json.Encode.encode 4

point : World.Point -> Value
point pt =
    let {x, y} = Point2d.toMeters pt in
    object 
        [ ("x", float x)
        , ("y", float y)
        ]

polygon : World.Polygon -> Value
polygon = 
    Polygon2d.outerLoop
        >> list point

box : World.BoundingBox -> Value
box b =
    let {minX, maxX, minY, maxY} = BoundingBox2d.extrema b
        pt1 = Point2d.xy minX minY
        pt2 = Point2d.xy maxX maxY
    in
    object
        [ ("1", point pt1)
        , ("2", point pt2)
        ]


shadows : Dict String Shadow -> Value
shadows =
    dict identity (Map.Shadow.polygon >> polygon)
    
objeto : Object -> Value
objeto {kind} =
    case kind of
        Wall pt1 pt2 -> 
            object 
                [ ("kind", string "wall")
                , ("1", point pt1)
                , ("2", point pt2)
                ]

objects : Dict String Object -> Value
objects =
    dict identity objeto

lights : Dict String Light -> Value
lights =
    dict identity (Map.Light.point >> point)

room : Room -> Value
room r =
    object
        [ ("lights", Map.Room.lights r |> lights)
        , ("objects", Map.Room.objects r |> objects)
        , ("shadows", Map.Room.shadows r |> shadows)
        , ("box", Map.Room.boundingBox r |> box)
        ]

rooms : Dict String Room -> Value
rooms =
    dict identity room

level : Level -> Value
level l =
    object
        [ ("name", string l.name)
        , ("rooms", rooms l.rooms)
        ]
