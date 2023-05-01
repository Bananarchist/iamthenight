module Map.Decode exposing (..)

import BoundingBox2d
import Dict exposing (Dict)
import Json.Decode exposing (..)
import Map.Entity exposing (Entity(..))
import Map.Level exposing (Level)
import Map.Light exposing (Light)
import Map.Object exposing (Object)
import Map.Room exposing (Room)
import Map.Shadow exposing (Shadow)
import Map.World as World
import Point2d
import Polygon2d


decode : String -> Result String Level
decode =
    Json.Decode.decodeString level
        >> Result.mapError errorToString


point : Decoder World.Point
point =
    map2 Point2d.meters
        (field "x" float)
        (field "y" float)


polygon : Decoder World.Polygon
polygon =
    map Polygon2d.singleLoop (list point)


box : Decoder World.BoundingBox
box =
    map2 BoundingBox2d.from
        (field "1" point)
        (field "2" point)


shadow : Decoder Shadow
shadow =
    map Map.Shadow.new polygon


shadows : Decoder (Dict String Shadow)
shadows =
    dict shadow


entity : Decoder Entity
entity =
    let
        entityHelper kind =
            case kind of
                "object" ->
                    map EObject (field "data" objeto)

                "light" ->
                    map ELight (field "data" light)

                "shadow" ->
                    map EShadow (field "data" shadow)

                _ ->
                    fail ("Unknown object type " ++ kind)
    in
    field "kind" string
        |> andThen entityHelper


entities : Decoder (Dict String Entity)
entities = 
    dict entity

objeto : Decoder Object
objeto =
    let
        objetoHelper kind =
            case kind of
                "wall" ->
                    map2 Map.Object.newWall (field "1" point) (field "2" point)

                _ ->
                    fail ("Unknown object type " ++ kind)
    in
    field "kind" string
        |> andThen objetoHelper


objects : Decoder (Dict String Object)
objects =
    dict objeto


light : Decoder Light
light =
    map Map.Light.newPoint point


lights : Decoder (Dict String Light)
lights =
    dict light


room : Decoder Room
room =
    map3
        (\b e n ->
            Map.Room.newRoom b
                |> Map.Room.setEntities e
                |> Map.Room.setEntry n
        )
        (field "box" box)
        (field "entities" entities)
        (field "entry" string)


rooms : Decoder (Dict String Room)
rooms =
    dict room


level : Decoder Level
level =
    map3 Level
        (field "name" string)
        (field "entry" string)
        (field "rooms" rooms)
