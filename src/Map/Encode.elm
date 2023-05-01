module Map.Encode exposing (..)

import BoundingBox2d
import Dict exposing (Dict)
import Json.Encode exposing (..)
import Map.Entity exposing (Entity(..))
import Map.Level exposing (Level)
import Map.Light exposing (Light)
import Map.Object exposing (Object, ObjectKind(..))
import Map.Room exposing (Room)
import Map.Shadow exposing (Shadow)
import Map.World as World
import Point2d
import Polygon2d


encode : Level -> String
encode =
    level
        >> Json.Encode.encode 4


point : World.Point -> Value
point pt =
    let
        { x, y } =
            Point2d.toMeters pt
    in
    object
        [ ( "x", float x )
        , ( "y", float y )
        ]


polygon : World.Polygon -> Value
polygon =
    Polygon2d.outerLoop
        >> list point


box : World.BoundingBox -> Value
box b =
    let
        { minX, maxX, minY, maxY } =
            BoundingBox2d.extrema b

        pt1 =
            Point2d.xy minX minY

        pt2 =
            Point2d.xy maxX maxY
    in
    object
        [ ( "1", point pt1 )
        , ( "2", point pt2 )
        ]


shadows : Dict String Shadow -> Value
shadows =
    dict identity (Map.Shadow.polygon >> polygon)


objeto : Object -> Value
objeto { kind } =
    case kind of
        Wall pt1 pt2 ->
            object
                [ ( "kind", string "wall" )
                , ( "1", point pt1 )
                , ( "2", point pt2 )
                ]


objects : Dict String Object -> Value
objects =
    dict identity objeto


entities : Dict String Entity -> Value
entities =
    dict identity entity


lights : Dict String Light -> Value
lights =
    dict identity (Map.Light.point >> point)


entity : Entity -> Value
entity e =
    (case e of
        EObject o ->
            [ ( "kind", string "object" )
            , ( "data", objeto o )
            ]

        ELight l ->
            [ ( "kind", string "light" )
            , ( "data", (Map.Light.point >> point) l )
            ]

        EShadow s ->
            [ ( "kind", string "shadow" )
            , ( "data", (Map.Shadow.polygon >> polygon) s )
            ]
    )
        |> object


room : Room -> Value
room r =
    object
        [ ( "entities", Map.Room.entities r |> entities )
        , ( "box", Map.Room.boundingBox r |> box )
        , ( "entry", Map.Room.entryId r |> string )
        ]


rooms : Dict String Room -> Value
rooms =
    dict identity room


level : Level -> Value
level l =
    object
        [ ( "name", string l.name )
        , ( "rooms", rooms l.rooms )
        , ( "entry", string l.entry )
        ]
