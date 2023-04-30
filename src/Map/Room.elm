module Map.Room exposing (..)
import Length

import Dict exposing (Dict)
import Map.Object exposing (Object)
import Map.Light exposing (Light)
import Map.World as World
import BoundingBox2d
import Point2d
import Map.Shadow exposing (Shadow)
import Aviary.Birds exposing (cardinal)
import Aviary.Birds exposing (becard)
import Aviary.Birds exposing (bluebird)
import Helpers exposing (anyPredicates)
import Aviary.Birds exposing (applicator)
import Helpers exposing (duple)
import Helpers exposing (mapTuple)
import Helpers exposing (uncurry)
import Polygon2d
import Helpers exposing (pointBetweenか)
import Direction2d
import Vector2d
import LineSegment2d
import Length
import Helpers exposing (maybeTuple)
import Parameter1d
import List.Extra
import Quantity
import Rectangle2d
import Helpers exposing (tupleList)
import Result.Extra
import Frame2d
import Polyline2d
import Array exposing (Array)
import Area
import Hash exposing (Hash)

type alias LightId = String
type alias ObjectId = String
type alias ShadowId = String
type alias RoomId = String

type Room 
    = Room RoomData PendingRoomData

type Path
    = StartingAt ShadowId
    | ToRoom
    | TerminatingAt ShadowId
    | Between ShadowId ShadowId

type alias RoomData =
    { objects : Dict ObjectId Object
    , lights : Dict LightId Light
    , shadows : Dict ShadowId Shadow
    -- move to map editor
    , rays : Dict LightId (List World.Line)
    , box : World.BoundingBox
    , shadow_to_shadow : Dict ShadowId (List ShadowId, List ShadowId)
    , initial : (ShadowId, Shadow)
    , terminal : (ShadowId, Shadow)
    }
-- Move to map editor    
type alias PendingRoomData =
    { objects : List Object
    , lights : List Light
    , shadows : List Shadow
    }

defaultPendingRoomData =
    { objects = [], shadows = [], lights = []}

roomPendingData : Room -> Bool
roomPendingData (Room _ p) = 
    --[.objects, .shadows, .lights]
    --|> List.map (becard not List.isEmpty)
    [ .objects >> List.isEmpty >> not
    , .shadows >> List.isEmpty >> not
    , .lights >> List.isEmpty >> not
    ]
    |> cardinal anyPredicates p
    

newRoom : World.BoundingBox -> Room
newRoom bb =
    Room
        { objects = Dict.empty
        , lights = Dict.empty
        , shadows = Dict.empty
        , rays = Dict.empty
        , box = bb
        }
        defaultPendingRoomData

defaultRoom : Room
defaultRoom =
    BoundingBox2d.from Point2d.origin (Point2d.meters 15 10) 
    |> newRoom

copyToNewRoom : List String -> Room -> Room
copyToNewRoom keys (Room d pd) =
    Room 
        { d
        | objects = Dict.filter (\k _ -> List.member k keys) d.objects
        , lights = Dict.filter (\k _ -> List.member k keys) d.lights
        }
        pd
    |> createShadows

clearRoom : Room -> Room
clearRoom (Room d pd) =
    Room
    { d
    | objects = Dict.empty
    , lights = Dict.empty
    , shadows = Dict.empty
    }
    { pd
    | objects = []
    , lights = []
    , shadows = []
    }

{-| Preserves room occupants not in second room -}
diff : Room -> Room -> Room
diff (Room d pd) (Room ds _) =
    Room
    { d
    | objects = Dict.diff d.objects ds.objects
    , lights = Dict.diff d.lights ds.lights
    , rays = Dict.diff d.rays ds.rays
    }
    pd


union : Room -> Room -> Room
union (Room d pd) (Room ds pds) =
    Room
    { d
    | objects = Dict.union ds.objects d.objects
    , lights = Dict.union ds.lights d.lights
    , rays = Dict.union ds.rays d.rays
    }
    pd


roomData : Room -> RoomData
roomData (Room d _) = d

pendingRoomData : Room -> PendingRoomData
pendingRoomData (Room _ d) = d

objects : Room -> Dict String Object
objects = roomData >> .objects

setObjects : Dict String Object -> Room -> Room
setObjects o (Room d pd) =
    Room {d | objects = o} pd

mapObjects : (Dict String Object -> Dict String Object) -> Room -> Room
mapObjects fn r =
    objects r
    |> fn
    |> cardinal setObjects r

lights : Room -> Dict String Light
lights = roomData >> .lights

rays : Room -> Dict String (List World.Line)
rays = roomData >> .rays

setLights : Dict String Light -> Room -> Room
setLights o (Room d pd) =
    Room {d | lights = o} pd

mapLights : (Dict String Light -> Dict String Light) -> Room -> Room
mapLights fn r =
    lights r
    |> fn
    |> cardinal setLights r

anyLightsか : Room -> Bool
anyLightsか = lights >> Dict.size >> (/=) 0


shadows : Room -> Dict String Shadow
shadows = roomData >> .shadows


setShadows : Dict String Shadow -> Room -> Room
setShadows o (Room d pd) =
    Room {d | shadows = o} pd

boundingBox : Room -> World.BoundingBox
boundingBox = roomData >> .box

setBoundingBox : World.BoundingBox -> Room -> Room
setBoundingBox o (Room d pd) =
    Room {d | box = o} pd

pendingObjects : Room -> List Object
pendingObjects = pendingRoomData >> .objects

pendingLights : Room -> List Light
pendingLights = pendingRoomData >> .lights

pendingShadows : Room -> List Shadow
pendingShadows = pendingRoomData >> .shadows

occupants : Room -> Int
occupants r =
    (objects r |> Dict.size)
    + (lights r |> Dict.size)
    + (shadows r |> Dict.size)

pendingOccupants : Room -> Int
pendingOccupants r =
    (pendingObjects r |> List.length)
    + (pendingLights r |> List.length)
    + (pendingShadows r |> List.length)

addKey : String -> Room -> Room
addKey key room = 
    if wantingKeys room == 0 then
        room
    else if not <| List.isEmpty (pendingObjects room) then
        keyObject key room
    else if not <| List.isEmpty (pendingLights room) then
        keyLight key room
    else 
        keyShadow key room

dropObject : String -> Room -> Room
dropObject key (Room d pd) =
    Room { d | objects = Dict.remove key d.objects } pd

dropLight : String -> Room -> Room
dropLight key (Room d pd) =
    Room { d | lights = Dict.remove key d.lights, rays = Dict.remove key d.rays } pd

createObject : Object -> Room -> Room
createObject obj (Room d pd) =
    Room d { pd | objects = obj :: pd.objects }

keyObject : String -> Room -> Room
keyObject key (Room d pd) =
    case pd.objects of
        ( o :: os) -> 
            Room 
                { d 
                | objects = Dict.insert key o d.objects 
                }
                { pd 
                | objects = os
                }
        _ -> Room d pd


createLight : Light -> Room -> Room
createLight light (Room d pd) =
    Room d { pd | lights = light :: pd.lights }

rayForLight : Light -> Room -> List (World.Line)
rayForLight l (Room d _) =
    let 
        (roomEdges, roomPts) = 
            d.box
            |> duple
            |> mapTuple Rectangle2d.fromBoundingBox
            |> Tuple.mapBoth Rectangle2d.edges Rectangle2d.vertices

        clipLineToRoomEdge line =
            List.filterMap (LineSegment2d.intersectionPoint line) roomEdges
            |> List.map (LineSegment2d.from cpoint)
            |> List.head
        cpoint = Map.Light.point l
        objVerts = Dict.foldl (\_ v a -> Map.Object.polygon v |> Polygon2d.outerLoop |> (++) a) [] d.objects
    in
    cardinal List.filterMap (objVerts ++ roomPts)
        (\p -> 
            Maybe.andThen (\dir -> 
                let v = Vector2d.withLength (Length.meters 10000) dir
                in clipLineToRoomEdge (LineSegment2d.fromPointAndVector cpoint v)
            ) 
            (Direction2d.from cpoint p)
        )


updateRays : Room -> Room
updateRays ((Room d pd) as room) =
    Room
    { d
    | rays = cardinal Dict.map d.lights
        (\_ l -> rayForLight l room)
    }
    pd
    

keyLight : String -> Room -> Room
keyLight key ((Room d pd) as room) =
    case pd.lights of
        ( l :: ls) -> 
            Room 
                { d 
                | lights = Dict.insert key l d.lights 
                , rays = Dict.insert key (rayForLight l room) d.rays
                }
                { pd 
                | lights = ls
                }
        _ -> Room d pd


createShadow : Shadow -> Room -> Room
createShadow shadow (Room d pd) =
    Room d { pd | shadows = shadow :: pd.shadows }

keyShadow : String -> Room -> Room
keyShadow key (Room d pd) =
    case pd.shadows of
        ( sh :: shs) -> 
            Room 
                { d 
                | shadows = Dict.insert key sh d.shadows 
                }
                { pd 
                | shadows = shs
                }
        _ -> Room d pd


wantingKeys : Room -> Int
wantingKeys (Room _ pd) = 
    [ .objects >> List.length
    , .shadows >> List.length
    , .lights >> List.length
    ]
    |> List.map ((|>) pd)
    |> List.sum


keyUsed : String -> Room -> Bool
keyUsed k room =
    (Dict.member k (objects room))
    || (Dict.member k (lights room))
    || (Dict.member k (shadows room))

createShadows : Room -> Room
createShadows ((Room d pd) as room) =
    let
        luces = 
            room
            |> duple
            |> Tuple.mapBoth pendingLights (lights >> Dict.values)
            |> uncurry (++)
        objetos =
            room
                |> duple
                |> Tuple.mapBoth pendingObjects (objects >> Dict.values)
                |> uncurry (++) 
    in
    if List.length luces == 0 || List.length objetos == 0 then
        room 

    else
        let
            polys : List World.Polygon
            polys =
                List.map Map.Object.polygon objetos

            lightPts =
                List.map Map.Light.point luces

            lightToObj lightPt poly =
                let
                    (roomEdges, roomPts) = 
                        d.box
                        |> duple
                        |> mapTuple Rectangle2d.fromBoundingBox
                        |> Tuple.mapBoth Rectangle2d.edges Rectangle2d.vertices

                    clipLineToRoomEdge line =
                        List.filterMap (LineSegment2d.intersectionPoint line) roomEdges
                        |> List.map (LineSegment2d.from (LineSegment2d.startPoint line))
                        |> List.head

                    objPts =
                        Polygon2d.outerLoop poly

                    tests =
                        List.map (pointBetweenか lightPt) objPts
                            |> List.map ((<<) not)

                    lines =
                        cardinal List.filterMap (objPts ++ roomPts)
                            ( duple 
                            >> Tuple.mapBoth Just 
                                ( Direction2d.from lightPt 
                                >> Maybe.map (Vector2d.withLength (Length.meters 1000)) 
                                >> Maybe.map (LineSegment2d.fromPointAndVector lightPt)
                                >> Maybe.andThen clipLineToRoomEdge
                                ) 
                            >> maybeTuple
                            )

                    linePts =
                        d.rays
                        |> Dict.values
                        |> List.concatMap identity
                        |> List.map
                            (duple
                            >> Tuple.mapBoth (LineSegment2d.length >> Length.inCentimeters >> floor) LineSegment2d.interpolate
                            >> uncurry Parameter1d.steps
                            )
                            
                        {-
                        cardinal List.map
                            lines
                            (Tuple.mapSecond
                                (duple
                                >> Tuple.mapBoth (LineSegment2d.length >> Length.inCentimeters >> floor) LineSegment2d.interpolate
                                >> uncurry Parameter1d.steps
                                )
                            )
                        -}

                    shadedPts =
                        cardinal List.concatMap linePts
                            (\pts ->
                                List.Extra.dropWhile (cardinal Polygon2d.contains poly >> not) pts
                                |> List.Extra.dropWhile (\pt ->
                                    if Polygon2d.contains pt poly |> not then
                                        List.any (Point2d.equalWithin Length.centimeter pt) objPts
                                    else
                                        True
                                )

                                --|> Polyline2d.fromVertices |> Debug.log "polyline from verts"
                                --|> Polyline2d.segments |> Debug.log "polylen segs"
                                --|> List.concatMap (LineSegment2d.endpoints >> tupleList)
                            )
                        {-
                        cardinal List.concatMap
                            linePts
                            (\(v, pts) ->
                                let 
                                    d2v = Point2d.distanceFrom lightPt v
                                in
                                List.Extra.dropWhile (Point2d.distanceFrom lightPt >> Quantity.lessThanOrEqualTo d2v) pts
                                -->> List.Extra.dropWhile (cardinal Polygon2d.contains poly)
                            )
                        -}

                    --List.filter (\p -> List.foldl (\t a -> t p && a) True tests) pts |> Debug.log "filtered"
                    shadowPoly =
                        Polygon2d.convexHull shadedPts
                in
                shadowPoly

            -- sorts counterclockwise
            rotationalPointSort pts =
                let
                    quad p =
                        case mapTuple Quantity.greaterThanOrEqualToZero (Point2d.coordinates p) of
                            (True, True) -> 4
                            (True, False) -> 2
                            (False, True) -> 3
                            (False, False) -> 1
                in
                cardinal List.sortWith pts
                    (\pt1 pt2 ->
                        let 
                            ptcmp p1 p2 =
                                let (q1, q2) = (quad pt1, quad pt2) 
                                    comparison = compare q1 q2
                                in
                                if comparison /= GT then
                                    comparison 
                                else
                                    let
                                        midpoint = Point2d.midpoint p1 p2
                                        newFrame = Frame2d.atPoint midpoint 
                                    in
                                    ptcmp 
                                        (Point2d.relativeTo newFrame p1)
                                        (Point2d.relativeTo newFrame p1)
                        in
                        ptcmp pt1 pt2
                    )




            polygonUnion poly1 poly2 =
                let
                    p1verts = Polygon2d.outerLoop poly1
                in
                if List.map Polygon2d.contains p1verts
                    |> cardinal anyPredicates poly2
                then
                    p1verts
                    ++ Polygon2d.outerLoop poly2
                    |> rotationalPointSort
                    |> Polygon2d.singleLoop
                    |> Ok
                else
                    Err (poly1, poly2)

            mergeShadows sss =
                if List.length sss < 2 then
                    sss
                else
                    let 
                        merged = 
                            sss
                            |> List.Extra.uniquePairs
                            |> List.concatMap 
                                ( uncurry polygonUnion 
                                >> Result.Extra.unpack tupleList List.singleton
                                )
                    in
                    if List.length merged == List.length sss then
                        merged
                    else
                        mergeShadows merged

            newShadows =
                cardinal List.filterMap polys
                    ( Just >> Maybe.map2 lightToObj (List.head lightPts) )
                |> mergeShadows
                |> List.map Map.Shadow.new
        in
        Room { d | shadows = Dict.empty } { pd | shadows = newShadows }


{-
    determine direction from light to wall corners
    DO NOT BUILD A LINE SEGMENT
    Instead increment by one y, then one x, and test
        if counter is inside rect then terminate
        if counter has reached edge, then select that and the point of intersection along smae direction
            with edge of wall


    -- occlusion method
    make array with room dimensions set to SHADOWED
    fill in object indices in array as OPAQUE
    fill in light indices in array as LIT


    -- sweeping
    collect all points, sort by angle
    sweep light rays from light by increasing angle
    sweeping light rays holding state, initially "light state"
        - hitting an object vertex enters "shade sweep state"
        - hitting an object vertex again enters "check state"
        - hitting the wall in shade sweep state does nothing
        - hitting the wall in check state returns to "light state"
    vertices of objects or wall hit during shade sweep are collected



    edges of light-to-corner to wal

    


-}

type PointStatus
    = Opaque
    | Lit
    | Shadowed

occlusionMap : Room -> Array PointStatus
occlusionMap (Room d pd) =
    let
        (w, h) = d.box |> BoundingBox2d.dimensions
    in
    cardinal Array.repeat Shadowed
        (Quantity.times w h |> Area.inSquareCentimeters |> ceiling)
    |> \arr -> Dict.foldl 
        (\_ l a -> Array.set 
            (l |> Map.Light.point 
            |> Point2d.coordinates 
            |> mapTuple (Length.inCentimeters >> floor) 
            |> Tuple.mapSecond ((*) (Length.inCentimeters w |> floor)) 
            |> (uncurry (+))
            |> (+) -1 
            |> Debug.log "idx") 
            Lit a) 
        arr d.lights

pointId : World.Point -> Hash
pointId =
    Point2d.coordinates
    >> mapTuple (Length.inMeters >> Hash.fromFloat)
    >> uncurry Hash.dependent

segmentId : World.Line -> Hash
segmentId =
    LineSegment2d.endpoints
    >> mapTuple pointId
    >> uncurry Hash.independent
