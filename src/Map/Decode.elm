module Map.Decode exposing (..)

import Json.Decode exposing (..)
import Map.World as World
import Point2d
import Map.Light exposing (Light)
import Map.Room exposing (Room)
import Map.Shadow exposing (Shadow)
import Map.Object exposing (Object)
import Polygon2d
import Dict exposing (Dict)
import BoundingBox2d
import Map.Level exposing (Level)


decode : String -> Result String Level
decode = 
    Json.Decode.decodeString level
    >> Result.mapError errorToString

point : Decoder World.Point
point =
    map2 Point2d.meters
        (field "x" float)
        (field "y" float)

polygon : Decoder (World.Polygon)
polygon = 
    map Polygon2d.singleLoop (list point)

box : Decoder (World.BoundingBox)
box =
    map2 BoundingBox2d.from 
        (field "1" point)
        (field "2" point)


shadows : Decoder (Dict String Shadow)
shadows =
    dict (map Map.Shadow.new polygon)
    
objeto : Decoder Object
objeto =
    let
        objetoHelper kind =
            case kind of
                "wall" -> map2 Map.Object.newWall ( field "1" point ) ( field "2" point)
                _ -> fail ("Unknown object type " ++ kind)
    in
    field "kind" string
    |> andThen objetoHelper

objects : Decoder (Dict String Object)
objects =
    dict objeto

lights : Decoder (Dict String Light)
lights =
    dict (map Map.Light.newPoint point)

room : Decoder (Room)
room =
    map4 (\l o s b -> 
            Map.Room.newRoom b
            |> Map.Room.setLights l
            |> Map.Room.setObjects o
            |> Map.Room.setShadows s
        )
        (field "lights" lights)
        (field "objects" objects)
        (field "shadows" shadows)
        (field "box" box)

rooms : Decoder (Dict String Room)
rooms =
    dict room

level : Decoder Level
level =
    map2 Level
        (field "name" string)
        (field "rooms" rooms)
