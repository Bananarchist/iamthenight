port module Editor.Main exposing (main)
import Array
import Aviary.Birds exposing (robinStar)
import Svg
import Map.Light
import Length
import Map.Light exposing (Light)
import Map.Room exposing (defaultRoom)
import Map.Room exposing (newRoom)
import Map.Object exposing (Object)

import Aviary.Birds exposing (applicator, blackbird, cardinal, kestrel)
import BoundingBox2d
import Browser
import Circle2d
import Constants as C
import Dict exposing (Dict)
import Direction2d
import Frame2d
import Geometry.Svg
import Helpers exposing (anyPredicates, bool, bothか, duple, eitherか, elput, getDictValues, mapTuple, maybeTuple, pointBetweenか, selectDictKeys, uncurry)
import Html exposing (Html)
import Html.Attributes as Hats
import Html.Events.Extra.Mouse exposing (Event, onDown, onMove, onUp)
import LineSegment2d
import List.Extra
import Map.Encode as MapEncoder
import Map.Decode as MapDecoder
import Parameter1d exposing (steps)
import Pixels
import Point2d exposing (Point2d)
import Polygon2d
import Quantity exposing (Quantity)
import Random
import Rectangle2d exposing (Rectangle2d)
import Svg exposing (Svg)
import Svg.Attributes as Gats
import Uuid exposing (Uuid)
import Vector2d
import Map.Room as Room exposing (Room)
import Map.World as World
import Map.Shadow
import Aviary.Birds exposing (finchStar)
import Keyboard
import Keyboard exposing (Key)


type alias ObjectId = String
type alias RoomId = String
type alias PortalId = String
type alias Layers = List ( LayerId, Bool )


type LayerId
    = ShadowLayer
    | ObjectLayer
    | EdgesLayer
    | RoomLayer
    | RaysLayer


type alias Model =
    { mouseState : Tool
    , keys : List Key
    , activeRoom : Maybe RoomId
    , scale : Float
    , canvasWidth : C.ScreenLength
    , canvasHeight : C.ScreenLength
    , frame : C.Frame
    , roomNames : Dict RoomId String
    , rooms : Dict RoomId Room
    , layers : Layers
    }


labelForLayer : LayerId -> String
labelForLayer layer =
    case layer of
        ShadowLayer ->
            "Shadows"

        ObjectLayer ->
            "Objects"

        EdgesLayer ->
            "Graph Edges"

        RoomLayer ->
            "Boundary"
        RaysLayer ->
            "Rays"


toggleLayer : LayerId -> Layers -> Layers
toggleLayer k layers =
    if List.any (Tuple.first >> (==) k) layers then
        List.map
            (\( l, state ) ->
                if l == k then
                    ( l, not state )

                else
                    ( l, state )
            )
            layers

    else
        ( k, True ) :: layers


layerEnabledか : LayerId -> Layers -> Bool
layerEnabledか layerId layers =
    case List.Extra.find (Tuple.first >> (==) layerId) layers of
        Just ( _, x ) ->
            x

        Nothing ->
            False


enableLayer layer layers =
    if layerEnabledか layer layers then
        layers
    else toggleLayer layer layers

disableLayer layer layers =
    if layerEnabledか layer layers then
        toggleLayer layer layers
    else 
        case List.Extra.find (Tuple.first >> (==) layer) layers of
            Just _ -> layers
            Nothing -> (layer, False) :: layers

enableShadowLayer = enableLayer ShadowLayer
disableShadowLayer = disableLayer ShadowLayer
shadowLayerEnabledか = layerEnabledか ShadowLayer

enableObjectLayer = enableLayer ObjectLayer
disableObjectLayer = disableLayer ObjectLayer
objectLayerEnabledか = layerEnabledか ObjectLayer

enableEdgesLayer = enableLayer EdgesLayer
disableEdgesLayer = disableLayer EdgesLayer
edgesLayerEnabledか = layerEnabledか EdgesLayer

enableRoomLayer = enableLayer RoomLayer
disableRoomLayer = disableLayer RoomLayer
roomlayerEnabledか = layerEnabledか RoomLayer

enableRaysLayer = enableLayer RaysLayer
disableRaysLayer = disableLayer RaysLayer
rayslayerEnabledか = layerEnabledか RaysLayer


type alias ObjectData =
    { object : Object
    }


type Object
    = Light (Maybe C.ScreenPoint)
    | Wall (Maybe ( C.ScreenPoint, C.ScreenPoint ))
    | Shadow (List C.ScreenPoint) (Maybe C.ScreenPoint)


translateObject : C.ScreenVector -> Object -> Object
translateObject vec obj =
    case obj of
        Wall (Just points) ->
            points |> mapTuple (Point2d.translateBy vec) |> Just |> Wall

        Light (Just point) ->
            Point2d.translateBy vec point |> Just |> Light

        _ ->
            obj


boundingBoxForObject : Object -> C.ScreenBox
boundingBoxForObject obj =
    case obj of
        Wall (Just points) ->
            uncurry BoundingBox2d.from points

        Light (Just point) ->
            BoundingBox2d.singleton point

        _ ->
            BoundingBox2d.singleton Point2d.origin



port downloadData : String -> Cmd msg
port toggleDialog : String -> Cmd msg
port fileDataReceiver : (String -> msg) -> Sub msg
port createOcclusionCanvas : Array.Array Bool -> Cmd msg

type Msg
    = SelectTool Tool
    | ApplyTool Event
    | ApplyToolTo Selection Event
    | FinishTool
    | TrackTool Event
    | Zoom Float
    | ToggleLayer LayerId
    | TriggerFialog
    | ImportData (Result String (Dict String Room))
    | ExportData
    | SelectRoom RoomId
    | CreateRoom
    | RenameRoom RoomId String
    | ResizeRoom 
    | GeneratedRoomId (List Room) (List Uuid)
    | GeneratedObjectIds RoomId (List Uuid)
    | KeyboardMsg Keyboard.Msg



type Tool
    = CreationTool Object
    | SelectionTool Selection
    | MoveCanvasTool (Maybe C.ScreenPoint)
    | MoveTool Selection (Maybe ( C.ScreenPoint, C.ScreenPoint ))


type alias Selection = Room
{- { objects : List String
    , lights : List String
    , shadows : List String
    }
-}
    
{- { objects : List ObjectId
    , boundingBox : Maybe C.ScreenBox
    }
-}


emptySelection : Selection
emptySelection =
    Room.defaultRoom


{-
selectionForObjects : Dict ObjectId ObjectData -> Selection
selectionForObjects objects =
    { objects = Dict.keys objects
    , boundingBox = Dict.values objects |> List.map (.object >> boundingBoxForObject) |> BoundingBox2d.aggregateN
    }
-}



{- objectsForSelection -}


init : { width : Int, height : Int } -> ( Model, Cmd Msg )
init { width, height } =
    let
        midY =
            height // 2 |> toFloat
        midX =
            (width - 120) // 2 |> toFloat
        rooms = 
            [ defaultRoom 
            |> Room.createObject 
                (Map.Object.newWall 
                    (Point2d.centimeters (midX * 0.5) (midY * 0.8)) 
                    (Point2d.centimeters (midX * 0.6) (midY * 1.2)) 
                )
            |> Room.createLight 
                (Map.Light.newPoint
                   (Point2d.centimeters (midX * 0.2) midY)
                )
            ]

        beshadowedRooms = rooms --List.map Room.createShadows rooms

        mod =
            { mouseState = SelectionTool emptySelection
            , keys = []
            , activeRoom = Nothing 
            , scale = 1
            , canvasWidth = width |> toFloat |> cardinal (-) 100 |> Pixels.float
            , canvasHeight = height |> toFloat |> Pixels.float
            , frame = Frame2d.atOrigin |> Frame2d.reverseY
            , roomNames = Dict.empty
            , rooms = Dict.empty
            , layers = [] |> enableObjectLayer |> enableShadowLayer |> enableRoomLayer |> disableEdgesLayer |> enableRaysLayer
            }
    in
    mod
        |> cardinal Tuple.pair
            (Random.generate (GeneratedRoomId beshadowedRooms)
                (Random.list (List.length beshadowedRooms) Uuid.uuidGenerator)
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ fileDataReceiver (MapDecoder.decode >> ImportData)
        , Sub.map KeyboardMsg Keyboard.subscriptions
        ]

{-
updateCurrentRoom : (Room -> Room) -> Model -> Maybe (Room, (Model, Cmd Msg))
updateCurrentRoom mapper model =
    case model.activeRoom of
        Just id ->
            let
                mbRoom =
                    Dict.get id model.rooms
                    |> Maybe.map mapper
                    |> Maybe.map Room.createShadows

                mbRndm =
                    mbRoom
                    |> Maybe.map Room.wantingKeys
                    |> Maybe.map (cardinal Random.list Uuid.uuidGenerator)

                mbCmd =
                    model.activeRoom
                    |> Maybe.map GeneratedObjectIds
                    |> Maybe.map2 
                        (cardinal Random.generate)
                        mbRndm
            in
            { model 
            | mouseState = MoveTool selection Nothing 
            , rooms = Maybe.map (finchStar Dict.update model.rooms (Maybe.andThen (kestrel mbRoom))) model.activeRoom |> Maybe.withDefault model.rooms
            }
            |> cardinal Tuple.pair (Maybe.withDefault Cmd.none mbCmd)
        Nothing -> 
            Nothing

-}

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        wnn =
            elput Cmd.none
    in
    case msg of
        TriggerFialog ->
            (model, toggleDialog "fialog")
        ImportData (Err e) -> 
            model
            |> wnn
        ImportData (Ok data) ->
            if Dict.size data > 0 then
                { model
                | rooms = data
                , activeRoom = Dict.keys data |> List.head
                }
                |> wnn
            else 
                model |> wnn
        ExportData ->
            MapEncoder.encode model.rooms
            |> downloadData
            |> Tuple.pair model
        SelectRoom roomId ->
            { model | activeRoom = Just roomId }
            |> wnn
        ToggleLayer layer ->
            { model
                | layers = toggleLayer layer model.layers
            }
                |> wnn
        Zoom factor ->
            { model
                | scale = model.scale * factor
            }
                |> wnn
        KeyboardMsg km ->
            let
                (newKeys, mbKeyChange) = Keyboard.updateWithKeyChange 
                    (Keyboard.oneOf [Keyboard.editingKey, Keyboard.whitespaceKey]) km model.keys
                    -- add key for finishing shadow paths
                updateKeys m = { m | keys = newKeys }
                deleteSelection m =
                    m.activeRoom
                    |> duple
                    |> Tuple.mapSecond (Maybe.andThen (cardinal Dict.get m.rooms))
                    |> maybeTuple
                    |> Maybe.map (\(id, room) ->
                        let
                            newRoom = Room.diff room (getSelection m) -- |> Room.createShadows

                            rndm =
                                Room.wantingKeys room
                                |> cardinal Random.list Uuid.uuidGenerator

                            cmd =
                                Random.generate (GeneratedObjectIds id) rndm
                        in
                        { m 
                        | rooms = Dict.update id (kestrel (Just newRoom)) m.rooms
                        , mouseState = case m.mouseState of
                            SelectionTool _ -> 
                                Room.defaultRoom 
                                |> Room.setBoundingBox (Room.boundingBox room) 
                                |> SelectionTool
                            MoveTool selection pts ->
                                Room.defaultRoom 
                                |> Room.setBoundingBox (Room.boundingBox room) 
                                |> cardinal MoveTool pts
                            _ -> m.mouseState
                        }
                        |> cardinal Tuple.pair cmd
                    )
                    |> Maybe.withDefault (m, Cmd.none)

            in
            case mbKeyChange of
                Just (Keyboard.KeyDown Keyboard.Delete) ->
                    updateKeys model
                    |> deleteSelection 
                Just (Keyboard.KeyDown Keyboard.Backspace) ->
                    updateKeys model
                    |> deleteSelection
                Just (Keyboard.KeyDown Keyboard.Enter) -> 
                    case model.mouseState of
                        CreationTool (Shadow ps _) ->
                            let
                                shadow = 
                                    ps
                                        |> Polygon2d.convexHull
                                        |> screenPolyToWorldPoly model
                                        |> Map.Shadow.new
                                mbRoom =
                                    model.activeRoom
                                    |> Maybe.andThen (cardinal Dict.get model.rooms)
                                    |> Maybe.map (Room.createShadow shadow)
                                    --|> Maybe.map Room.createShadows

                                mbRndm =
                                    mbRoom
                                    |> Maybe.map Room.wantingKeys
                                    |> Maybe.map (cardinal Random.list Uuid.uuidGenerator)

                                mbCmd =
                                    model.activeRoom
                                    |> Maybe.map GeneratedObjectIds
                                    |> Maybe.map2 
                                        (cardinal Random.generate)
                                        mbRndm
                            in
                            { model 
                            | mouseState = SelectionTool emptySelection 
                            , rooms = Maybe.map (finchStar Dict.update model.rooms (Maybe.andThen (kestrel mbRoom))) model.activeRoom |> Maybe.withDefault model.rooms
                            }
                            |> updateKeys
                            |> cardinal Tuple.pair (Maybe.withDefault Cmd.none mbCmd)

                        _ -> model |> updateKeys |> wnn
                                    
                _ ->
                    updateKeys model
                    |> wnn
        GeneratedRoomId rooms ids ->
            let
                roomIds = List.map Uuid.toString ids
                newRooms = 
                    List.map2 Tuple.pair roomIds rooms 
                    |> List.foldl (uncurry Dict.insert) model.rooms
                roomsWantingKeys = 
                    (Dict.map (kestrel Room.wantingKeys) newRooms
                        |> Dict.filter (kestrel ((<) 0)))
            in
            { model 
            | rooms = newRooms
            , activeRoom = 
                if Dict.size model.rooms == 0 then 
                    List.head roomIds 
                else model.activeRoom
            }
            |> cardinal Tuple.pair
                (Cmd.batch 
                    (Dict.foldl 
                        (\k v acc -> 
                            Random.generate (GeneratedObjectIds k)
                                (Random.list v Uuid.uuidGenerator)
                            :: acc)
                        [] roomsWantingKeys)
                )
        GeneratedObjectIds roomid ids ->
            let
                mbRoom = Dict.get roomid model.rooms
                objIds = List.map Uuid.toString ids
                mbNewRoom =
                    Maybe.map (cardinal (List.foldl Room.addKey) objIds) mbRoom
                    {- occlusion = 
                    cardinal Maybe.map mbNewRoom
                        (\room -> Room.occlusionMap room |> Array.map (\ps ->
                            case ps of
                                Room.Shadowed -> True
                                Room.Opaque -> True
                                Room.Lit -> False
                            )
                        )
                    |> Maybe.withDefault Array.empty
                    -}
            in
            { model
            | rooms = 
                Maybe.map (cardinal (Dict.insert roomid) model.rooms) mbNewRoom 
                    |> Maybe.withDefault model.rooms
            }
            --|> cardinal Tuple.pair (createOcclusionCanvas occlusion)
            |> wnn

        SelectTool t ->
            { model
                | mouseState = t
            }
                |> wnn

        TrackTool evt ->
            let
                scaled =
                    evt.clientPos
                        |> mapTuple (cardinal (/) model.scale)
            in
            case model.mouseState of
                MoveCanvasTool (Just sp) ->
                    let
                        ep = uncurry Point2d.pixels evt.clientPos
                        vec = Vector2d.from sp ep |> Vector2d.placeIn (model.frame |> Frame2d.reverseX |> Frame2d.reverseY)
                    in
                    { model 
                    | frame = Frame2d.translateBy vec model.frame
                    , mouseState = MoveCanvasTool (Just ep) 
                    }
                        |> wnn

                MoveTool objects (Just ( sp, _ )) ->
                    let
                        ep =
                            evt.clientPos |> uncurry Point2d.pixels
                        vec = 
                            Vector2d.from sp ep
                    in
                    { model | mouseState = MoveTool objects (Just ( sp, ep )) }
                        |> wnn

                CreationTool (Shadow ps mbPt) ->
                    { model
                    | mouseState = CreationTool (Shadow ps (evt.clientPos |> uncurry Point2d.pixels |> Just))
                    }
                    |> wnn
                CreationTool (Wall (Just ( sp, _ ))) ->
                    let
                        ep =
                            evt.clientPos |> uncurry Point2d.pixels
                    in
                    { model | mouseState = CreationTool (Wall (Just ( sp, ep ))) }
                        |> wnn

                _ ->
                    model
                        |> wnn

        ApplyToolTo newSelection evt ->
            case model.mouseState of
                SelectionTool selection ->
                    { model
                        | mouseState = SelectionTool newSelection
                        , rooms = model.activeRoom
                            |> Maybe.map (\id ->
                                Dict.update id 
                                    (Maybe.map (Room.union selection >> cardinal Room.diff newSelection))
                                    model.rooms
                                )
                            |> Maybe.withDefault model.rooms
                    }
                        |> wnn

                _ ->
                    model
                        |> wnn

        ApplyTool evt ->
            let
                scaled =
                    evt.clientPos
                        |> mapTuple (cardinal (/) model.scale)
                worldPt : World.Point
                worldPt =
                    scaled
                        |> uncurry Point2d.pixels 
                        |> Point2d.placeIn model.frame
                        |> Point2d.at (Quantity.rate Length.centimeter Pixels.pixel)

            in
            case model.mouseState of
                MoveCanvasTool Nothing ->
                        { model
                        | mouseState = MoveCanvasTool (Just (evt.clientPos |> uncurry Point2d.pixels))
                        }
                            |> wnn

                MoveCanvasTool (Just sp) ->
                    let
                        ep = uncurry Point2d.pixels evt.clientPos
                        vec = Vector2d.from sp ep |> Vector2d.placeIn (model.frame |> Frame2d.reverseX)
                    in
                    { model
                    | frame = Frame2d.translateBy vec model.frame
                    , mouseState = MoveCanvasTool Nothing
                    }
                        |> wnn

                MoveTool selection mbPoints ->
                    case mbPoints of
                        Just ( sp, ep ) ->
                            let
                                vec = Vector2d.from sp ep |> screenVectorToWorldVector model
                                objs = Room.objects selection
                                lights = Room.lights selection
                                mbRoom =
                                    model.activeRoom
                                    |> Maybe.andThen (cardinal Dict.get model.rooms)
                                    |> Maybe.map (cardinal Room.diff selection)
                                    --|> Maybe.map Room.createShadows

                                mbRndm =
                                    mbRoom
                                    |> Maybe.map Room.wantingKeys
                                    |> Maybe.map (cardinal Random.list Uuid.uuidGenerator)

                                mbCmd =
                                    model.activeRoom
                                    |> Maybe.map GeneratedObjectIds
                                    |> Maybe.map2 
                                        (cardinal Random.generate)
                                        mbRndm
                            in
                            { model 
                            | mouseState = MoveTool selection Nothing 
                            , rooms = Maybe.map (finchStar Dict.update model.rooms (Maybe.andThen (kestrel mbRoom))) model.activeRoom |> Maybe.withDefault model.rooms
                            }
                            |> cardinal Tuple.pair (Maybe.withDefault Cmd.none mbCmd)

                        Nothing ->
                            { model
                                | mouseState = scaled |> uncurry Point2d.pixels |> duple |> Just |> MoveTool selection
                            }
                                |> wnn

                CreationTool (Shadow ps mbPoint) ->
                    { model
                    | mouseState = CreationTool (Shadow (evt.clientPos |> uncurry Point2d.pixels |> cardinal (::) ps) mbPoint)
                    }
                    |> wnn


                CreationTool (Light _) ->
                    -- new lights and walls create new shadows...
                    let
                        mbRoom =
                            model.activeRoom
                            |> Maybe.andThen (cardinal Dict.get model.rooms)
                            |> Maybe.map (Room.createLight (Map.Light.newPoint worldPt))
                            --|> Maybe.map Room.createShadows

                        mbRndm =
                            mbRoom
                            |> Maybe.map Room.wantingKeys
                            |> Maybe.map (cardinal Random.list Uuid.uuidGenerator)

                        mbCmd =
                            model.activeRoom
                            |> Maybe.map GeneratedObjectIds
                            |> Maybe.map2 
                                (cardinal Random.generate)
                                mbRndm
                    in
                    { model 
                    | mouseState = SelectionTool emptySelection 
                    , rooms = Maybe.map (finchStar Dict.update model.rooms (Maybe.andThen (kestrel mbRoom))) model.activeRoom |> Maybe.withDefault model.rooms
                    }
                    |> cardinal Tuple.pair (Maybe.withDefault Cmd.none mbCmd)

                CreationTool (Wall (Just points)) ->
                    let
                        (worldStartPt, worldEndPt) =
                            points
                                |> mapTuple (Point2d.placeIn model.frame)
                                |> mapTuple (Point2d.at (Quantity.rate Length.centimeter Pixels.pixel))
                        mbRoom =
                            model.activeRoom
                            |> Maybe.andThen (cardinal Dict.get model.rooms)
                            |> Maybe.map (Room.createObject (Map.Object.newWall worldStartPt worldEndPt)) -- need to convert Just points to worldPts
                            --|> Maybe.map Room.createShadows

                        mbRndm =
                            mbRoom
                            |> Maybe.map Room.wantingKeys
                            |> Maybe.map (cardinal Random.list Uuid.uuidGenerator)

                        mbCmd =
                            model.activeRoom
                            |> Maybe.map GeneratedObjectIds
                            |> Maybe.map2 
                                (cardinal Random.generate)
                                mbRndm
                    in
                    { model 
                    | mouseState = SelectionTool emptySelection 
                    , rooms = Maybe.map (finchStar Dict.update model.rooms (Maybe.andThen (kestrel mbRoom))) model.activeRoom |> Maybe.withDefault model.rooms
                    }
                        |> cardinal Tuple.pair (Maybe.withDefault Cmd.none mbCmd)

                CreationTool (Wall Nothing) ->
                    let
                        sp =
                            scaled |> uncurry Point2d.pixels

                        ep =
                            scaled
                                |> (\( x, y ) ->
                                        let
                                            x1 =
                                                bool (x + 1) (x - 1) (x < 1)

                                            y1 =
                                                bool (y + 1) (y - 1) (y < 1)
                                        in
                                        Point2d.pixels x1 y1
                                   )
                    in
                    { model | mouseState = CreationTool (Wall (Just ( sp, ep ))) }
                        |> wnn

                _ ->
                    model
                        |> wnn

        _ ->
            model
                |> wnn

selectionEmptyか : Model -> Bool
selectionEmptyか model =
    case model.mouseState of
        SelectionTool selection ->
            Room.occupants selection |> (==) 0
        MoveTool selection _ ->
            Room.occupants selection |> (==) 0
        _ ->
            True


view : Model -> Html Msg
view model =
    [ viewWorkSpace model
    , viewToolBox model
    ]
--    [ Html.canvas [ Hats.width (model.canvasWidth |> Pixels.toFloat |> floor), Hats.height (model.canvasHeight |> Pixels.toFloat |> floor) ] [] ]
    |> Html.main_ []


cursor = 
    { move = [ Gats.class "cursor-moving" ]
    , cross = [ Gats.class "cursor-crosshair" ]
    , select = [ Gats.class "cursor-select" ]
    }

viewWorkSpace : Model -> Svg Msg
viewWorkSpace model =
    let
        mbRoom =
            model.activeRoom
            |> Maybe.andThen (cardinal Dict.get model.rooms)

        canvasClickEvents =
            case model.mouseState of
                CreationTool (Shadow [] _) ->
                    [ onDown ApplyTool ] ++ cursor.cross

                CreationTool (Shadow (_::_) _) ->
                    [ onDown ApplyTool, onMove TrackTool ] ++ cursor.cross

                CreationTool (Wall Nothing) ->
                    [ onDown ApplyTool ]

                CreationTool (Wall _) ->
                    [ onDown ApplyTool, onMove TrackTool ] ++ cursor.cross

                CreationTool (Light _) ->
                    [ onDown ApplyTool ]

                SelectionTool selection ->
                    if selectionEmptyか model then
                        onDown ApplyTool :: cursor.cross

                    else
                        onDown (kestrel (SelectTool (SelectionTool emptySelection))) :: cursor.cross
                MoveCanvasTool mbPoints ->
                    case mbPoints of
                        Nothing -> [ onDown ApplyTool ] ++ cursor.move
                        Just _ -> [ onUp ApplyTool, onMove TrackTool ] ++ cursor.move

                MoveTool selection mbPoints ->
                    case mbPoints of
                        Nothing ->
                            [ onDown ApplyTool ] ++ cursor.move

                        Just _ ->
                            if Room.occupants selection > 0 then
                                [ onUp ApplyTool, onMove TrackTool ] ++ cursor.move
                            else
                                [ onUp ApplyTool, onMove TrackTool ] ++ cursor.move

        viewBox =
            let
                ( x, y ) =
                    ("0", "0") --Frame2d.originPoint model.frame |> Point2d.coordinates |> mapTuple (Pixels.toFloat >> String.fromFloat)

                ( w, h ) =
                    ( model.canvasWidth, model.canvasHeight ) |> mapTuple (Pixels.toFloat >> String.fromFloat)
            in
            String.join " " [ x, y, w, h ]
                |> Gats.viewBox

        canvasAttributes =
            canvasClickEvents ++ [ Gats.id "canvas", viewBox ]
    in
    (duple >> Tuple.mapSecond getSelection >> uncurry (viewRoom True))
    :: (Maybe.map (cardinal (viewRoom False)) mbRoom
            |> Maybe.map List.singleton
            |> Maybe.withDefault [])
    ++ (Maybe.map viewCanvas mbRoom
        |> Maybe.map List.singleton
        |> Maybe.withDefault [])
    |> List.foldl ((|>) model >> (++)) [] 
    |> Svg.svg canvasAttributes

viewCanvas : Room -> Model -> List (Svg Msg)
viewCanvas room model =
    Geometry.Svg.polygon2d 
        [ Gats.id "room"
        , Gats.fill (if Room.lights room |> Dict.size |> (/=) 0 then "white" else "black")
        ] 
        (Room.boundingBox room 
            |> Rectangle2d.fromBoundingBox |> Rectangle2d.toPolygon |> worldPolyToScreenPoly model) |> List.singleton

{-
toolSelectedAttrs : String -> Model -> List (Svg.Attribute Msg)
toolSelectedAttrs key model =
    case model.mouseState of
        SelectionTool selection ->
            if selectionEmptyか model then
                    [ Gats.class "cursor-select"
                    , onDown (ApplyToolTo (Room.copyToNewRoom [key] room))
                    ]
                , transformations = []
                }

            else
                []

        MoveTool selection (Just _) ->
            [ Gats.class "being-moved" ]

        _ ->
            []
-}

viewRoom : Bool -> Model -> Room -> List (Svg Msg)
viewRoom isSelected model room =
    if (Room.occupants room) + (Room.pendingOccupants room) == 0 then
        []
    else
        let
            (canvas, tool) =
                if isSelected then 
                    duple (\_ _ _ -> [])
                else
                    ((\_ _ _ -> []), (\_ -> viewTool))
             
        in
        [ tool
        , viewRaysLayer
        , viewObjectLayer
        , viewLightsLayer
        , viewShadowsLayer
        , canvas
        ]
        |> List.foldr ((\f -> robinStar f model isSelected room) >> (++)) []
            --|> List.foldl ((|>) model >> (++)) []

        {-
    let
        third = cardinal kestrel >> kestrel
        second = cardinal (kestrel >> kestrel)
        
        mods : Dict String { attrs : List (Svg.Attribute Msg), transformations : List (o -> o) }
        mods = 
            Room.objects room
            |> Dict.map (\k v ->
                case model.mouseState of
                    SelectionTool selection ->
                        if selectionEmptyか model then
                            { attrs = 
                                [ Gats.class "cursor-select"
                                , onDown (ApplyToolTo (Room.copyToNewRoom [k] room))
                                ]
                            , transformations = []
                            }

                        else
                            { attrs =  []
                            , transformations = []
                            }

                    MoveTool selection (Just _) ->
                        { attrs = [ Gats.class "being-moved" ]
                        , transformations = []
                        }

                    _ ->
                        { attrs = []
                        , transformations = []
                        }
            )


        selectionAttrs : List (Svg.Attribute Msg) -> Dict String (List (Svg.Attribute Msg))
        selectionAttrs attrs = 
            (Dict.map (kestrel << kestrel attrs) (Room.objects room))
            |> Dict.union (Dict.map (kestrel << kestrel attrs) (Room.lights room))
            
        moveAttrs : List (Svg.Attribute Msg) -> Dict String (List (Svg.Attribute Msg))
        moveAttrs attrs = 
            (Dict.map (kestrel << kestrel attrs) (Room.objects room))
            |> Dict.union (Dict.map (kestrel << kestrel attrs) (Room.lights room))

        canvasObjectClickEvents =
            case model.mouseState of
                SelectionTool selection ->
                    if selectionEmptyか model then
                        selectionAttrs [ Gats.class "cursor-select" ]
                        |> Dict.map (\k v -> onDown (kestrel (SelectTool (SelectionTool (Room.copyToNewRoom [k] room)))) :: v) --(ApplyToolTo k) :: v)

                    else
                        selectionAttrs [ Gats.class "selected" ]
                        |> Dict.filter (\k _ -> List.member k ((Room.objects room |> Dict.keys) ++ (Room.lights room |> Dict.keys)))

                MoveTool selection (Just _) ->
                    moveAttrs [ Gats.class "selected" ]
                    |> Dict.filter (\k _ -> List.member k ((Room.objects room |> Dict.keys) ++ (Room.lights room |> Dict.keys)))

                _ ->
                    Dict.empty


        transformations = canvasObjectClickEvents
            --(\_ _ -> [ Gats.transform ("scale(" ++ String.fromFloat model.scale ++ ")") ]) :: canvasObjectClickEvents
        canvas =
            Geometry.Svg.polygon2d [ Gats.id "room", Gats.fill (if Room.lights room |> Dict.size |> (/=) 0 then "white" else "black")] (Room.boundingBox room |> Rectangle2d.fromBoundingBox |> Rectangle2d.toPolygon |> worldPolyToScreenPoly model) |> List.singleton
    in
        -}

viewRaysLayer : Bool -> Room -> Model -> List (Svg Msg)
viewRaysLayer isSelected room model =
    if rayslayerEnabledか model.layers then
        viewRoomRays isSelected room model
    else
        []



viewShadowsLayer : Bool -> Room -> Model -> List (Svg Msg)
viewShadowsLayer isSelected room model =
    if shadowLayerEnabledか model.layers then
        viewRoomShadows isSelected room model

    else
        []


viewObjectLayer : Bool -> Room -> Model -> List (Svg Msg)
viewObjectLayer isSelected room model =
    if objectLayerEnabledか model.layers then
        {-
        let
            case model.mouseState of
                SelectionTool selection ->
                    if selectionEmptyか model then
                        { attrs = 
                            [ Gats.class "cursor-select"
                            , onDown (ApplyToolTo (Room.copyToNewRoom [k] room))
                            ]
                        , transformations = []
                        }

                    else
                        { attrs =  []
                        , transformations = []
                        }

                MoveTool selection (Just _) ->
                    { attrs = [ Gats.class "being-moved" ]
                    , transformations = []
                    }

                _ ->
                    { attrs = []
                    , transformations = []
                            }
        in
        -}
        viewRoomObjects isSelected room model 

    else
        []

viewLightsLayer : Bool -> Room -> Model -> List (Svg Msg)
viewLightsLayer isSelected room model =
    viewRoomLights isSelected room model

viewTool : Room -> Model -> List (Svg Msg)
viewTool room model =
    case model.mouseState of
        CreationTool (Shadow ((_ :: _ :: _) as pts) mbPt) ->
            pts
            |> List.foldl (\pt (mbLastPoint, lines) ->
                Maybe.map (cardinal LineSegment2d.from pt >> List.singleton) mbLastPoint 
                |> Maybe.withDefault []
                |> cardinal (++) lines
                |> Tuple.pair (Just pt)
                ) (mbPt, [])
            |> Tuple.second
            |> List.map (Geometry.Svg.lineSegment2d (creationStyles model.mouseState ++ [Gats.class "shadow-line"]))

        CreationTool (Wall mbPts) ->
            mbPts
            |> Maybe.map 
                ( uncurry Rectangle2d.from 
                    >> Rectangle2d.toPolygon
                    >> Geometry.Svg.polygon2d (creationStyles model.mouseState)
                    >> List.singleton
                )
            |> Maybe.withDefault []
        _ ->
            []


creationStyles : Tool -> List (Svg.Attribute Msg)
creationStyles t =
    case t of
        CreationTool _ ->
            [ Gats.id "creating" ]

        _ ->
            []


baseToolEquivalence : Tool -> Tool -> Bool
baseToolEquivalence tool1 tool2 =
    case ( tool1, tool2 ) of
        ( SelectionTool _, SelectionTool _ ) ->
            True

        ( CreationTool (Wall _), CreationTool (Wall _) ) ->
            True

        ( CreationTool (Light _), CreationTool (Light _) ) ->
            True

        ( MoveTool _ _, MoveTool _ _ ) ->
            True

        ( MoveCanvasTool _, MoveCanvasTool _ ) ->
            True

        ( MoveCanvasTool _, MoveTool _ _ ) ->
            True

        ( MoveTool _ _, MoveCanvasTool _ ) ->
            True

        _ ->
            False


getSelection : Model -> Selection
getSelection { mouseState } =
    case mouseState of
        SelectionTool x ->
            x

        MoveTool x _ ->
            x

        _ ->
            emptySelection


viewToolBox : Model -> Html Msg
viewToolBox model =
    let
        creationTools =
            [ { label = "Create Wall", msg = SelectTool (CreationTool (Wall Nothing)), isSelected = baseToolEquivalence model.mouseState (CreationTool (Wall Nothing)) }
            , { label = "Create Light", msg = SelectTool (CreationTool (Light Nothing)), isSelected = baseToolEquivalence model.mouseState (CreationTool (Light Nothing)) }
            , { label = "Create Shadow", msg = SelectTool (CreationTool (Shadow [] Nothing)), isSelected = baseToolEquivalence model.mouseState (CreationTool (Light Nothing)) }

            ]
                |> toolSet

        controlTools =
            [ { label = "Selection", msg = SelectTool (SelectionTool emptySelection), isSelected = baseToolEquivalence model.mouseState (SelectionTool emptySelection) }
            ]
                |> toolSet

        transformationTools =
            let
                selection = getSelection model
                whichMove = if Room.occupants selection > 0 then MoveTool selection Nothing else MoveCanvasTool Nothing
            in
            [ { label = "Move", msg = SelectTool whichMove, isSelected = baseToolEquivalence model.mouseState (MoveTool emptySelection Nothing) }
            ]
                |> toolSet

        cmsPerPixel =
            [ if model.scale > 1 then "1" else (1 / model.scale) |> floor |> String.fromInt
            , "px:"
            , if model.scale < 1 then "10" else (10 * model.scale) |> floor |> String.fromInt
            , "cm"
            ] |> String.join ""
        zoomTools =
            [ { label = "-", msg = Zoom 0.5, isSelected = False }
            , { label = cmsPerPixel, msg = Zoom (1 / model.scale), isSelected = floor model.scale == 1 }
            , { label = "+", msg = Zoom 2, isSelected = False }
            ]
                |> toolSet
                |> Html.div [ Hats.id "zoomcontrols" ]
                |> List.singleton

        exportTools = 
            [ { label = "Import", msg = TriggerFialog, isSelected = False }
            , { label = "Export", msg = ExportData, isSelected = False }
            ]
            |> toolSet

        layerSelector =
            model.layers
                |> List.map
                    (\( l, checked ) ->
                        Html.div
                            [ Hats.class "display-check-box" ]
                            [ Html.input
                                [ Hats.type_ "checkbox"
                                , onDown (kestrel (ToggleLayer l))
                                , Hats.checked checked
                                ]
                                []
                            , l |> labelForLayer |> Html.text
                            ]
                    )
        roomTools =
            Dict.foldl 
                (\k _ acc -> 
                    Html.button [ onDown (kestrel (SelectRoom k)) ] 
                        [ Html.text (Dict.get k model.roomNames |> Maybe.withDefault k) ]
                        :: acc
                )
                [] model.rooms
            ++ [ Html.button [ onDown (kestrel CreateRoom) ] [ Html.text "New"] ]
            |> Html.div
                [ Hats.id "roomcontrols" ]
            |> List.singleton
                
    in
    List.Extra.zip
        (List.map (List.singleton >> Html.span [ Hats.class "controls-label" ]) [ Html.text "Controls"
        , Html.text "Objects"
        , Html.text "Transformations"
        , Html.text "Zoom"
        , Html.text "Display"
        , Html.text "Map Data"
        , Html.text "Rooms"
        ])
        [ controlTools
        , creationTools
        , transformationTools
        , zoomTools
        , layerSelector
        , exportTools
        , roomTools
        ]
        |> List.concatMap (uncurry (::))
        |> Html.div [ Hats.id "toolbox" ]


toolSet : List { label : String, msg : Msg, isSelected : Bool } -> List (Html Msg)
toolSet =
    List.concatMap toolButton


toolButton : { label : String, msg : Msg, isSelected : Bool } -> List (Html Msg)
toolButton { label, msg, isSelected } =
    [ Html.button
        [ Html.Events.Extra.Mouse.onDown (kestrel msg)
        , Hats.disabled isSelected
        ]
        [ Html.text label ]
    ]


viewRoomDataSet : (Room -> Dict String x) -> (Model -> List (Svg.Attribute Msg) -> x -> List (Svg Msg)) -> Room -> Model -> Dict String (List (Svg.Attribute Msg)) -> List (Svg Msg)
viewRoomDataSet dataSelector viewFn room model attrFns =
    room
        |> dataSelector 
        |> Dict.foldr (\k v a -> (Dict.get k attrFns |> Maybe.withDefault [] |> cardinal Tuple.pair v) :: a) []
        --|> List.sortWith (\(_, o1) (_, o2) -> objectViewSorter o1 o2)
        |> List.concatMap (uncurry (viewFn model))


type alias Viewer roomObject screenObject  =
    { roomMapper : Room -> Dict String roomObject
    , dynamicAttributes : String -> List (Svg.Attribute Msg)
    , staticAttributes : List (Svg.Attribute Msg)
    , objectMapper : roomObject -> screenObject
    , viewMapper : List (Svg.Attribute Msg) -> screenObject -> List (Svg Msg)
    }

viewRoomOccs : Viewer x y -> Room -> List (Svg Msg)
viewRoomOccs {roomMapper, dynamicAttributes, staticAttributes, objectMapper, viewMapper} room =
    room
    |> roomMapper
    |> Dict.foldr (\k v a -> (staticAttributes ++ (dynamicAttributes k), objectMapper v) :: a) []
    |> List.concatMap (uncurry viewMapper)

viewRoomOccupants : (Room -> Dict String x) -> (Model -> List (Svg.Attribute Msg) -> x -> List (Svg Msg)) -> Room -> Model -> List (Svg Msg)
viewRoomOccupants dataSelector viewFn room model =
    room
        |> dataSelector 
        |> Dict.foldr (\_ v a -> ([], v) :: a) [] 
        |> List.concatMap (uncurry (viewFn model))

viewRays : Room -> Model -> List (Svg Msg)
viewRays room model =
    room
    |> Room.rays
    |> Dict.values
    |> List.concatMap identity
    |> List.concatMap (viewRay model)

viewRay : Model -> World.Line -> List (Svg Msg)
viewRay model line =
    [ Geometry.Svg.lineSegment2d (rayStyles) (line |> worldLineToScreenLine model) ]

rayStyles = [ Gats.class "ray" ]

normalAttributes : Room -> Model -> (String -> List (Svg.Attribute Msg))
normalAttributes room model =
    case model.mouseState of
        SelectionTool selection ->
            if selectionEmptyか model then
                (\k -> [ onDown (Room.copyToNewRoom [k] room |> ApplyToolTo) ])
            else
                kestrel []
        _ -> kestrel []


selectedAttributes : Room -> Model -> (String -> List (Svg.Attribute Msg))
selectedAttributes room model =
    kestrel []
    {-
    case model.mouseState of
        SelectTool selection ->
            (\k -> if k 
        CreationTool (Wall mbPts) ->
            mbPts
            |> Maybe.map 
                ( uncurry Rectangle2d.from 
                    >> Rectangle2d.toPolygon
                    >> Geometry.Svg.polygon2d (creationStyles model.mouseState)
                    >> List.singleton
                )
            |> Maybe.withDefault []
        MoveTool selection (Just ( sp, ep )) ->
            if Point2d.equalWithin Pixels.pixel sp ep then
                []

            else
                let
                    vec = Vector2d.from sp ep |> screenVectorToWorldVector model
                    attrs = [ Gats.class "dragging", Gats.strokeDasharray "4 1" ]
                    transformations =
                        (Dict.map (kestrel << kestrel attrs) (Room.objects room))
                        |> Dict.union (Dict.map (kestrel << kestrel attrs) (Room.lights room))
                    lightTransformations = 
                        Dict.map (kestrel (Map.Light.translateLightByVector vec))
                    objTransformations = 
                        Dict.map (kestrel (Map.Object.translateObjectByVector vec))
                    roomModded = 
                        Room.mapObjects objTransformations selection
                        |> Room.mapLights lightTransformations
                in
                [ viewObjectLayer roomModded
                --, viewShadowsLayer transformations room
                , viewLightsLayer roomModded
                ]
                |> List.foldl ((|>) model >> (++)) []
        _ ->
            []
    (\k -> onDown (manage selection...))
    -}

selectedLightTransformations : Room -> Model -> C.ScreenCircle -> C.ScreenCircle
selectedLightTransformations = selectedTransformations Circle2d.translateBy

selectedObjectTransformations : Room -> Model -> C.ScreenPolygon -> C.ScreenPolygon
selectedObjectTransformations = selectedTransformations Polygon2d.translateBy

selectedShadowTransformations : Room -> Model -> C.ScreenPolygon -> C.ScreenPolygon
selectedShadowTransformations = selectedTransformations Polygon2d.translateBy

selectedRayTransformations : Room -> Model -> C.ScreenLine -> C.ScreenLine
selectedRayTransformations = selectedTransformations LineSegment2d.translateBy

selectedTransformations : (C.ScreenVector -> x -> x) -> Room -> Model -> x -> x
selectedTransformations vecTranslator room model value =
    case model.mouseState of
        MoveTool selection (Just (sp, ep)) ->
            if Point2d.equalWithin Pixels.pixel sp ep then value
            else
                let
                    vec = Vector2d.from sp ep
                in
                vecTranslator vec value
        _ ->
            value


viewRoomRays : Bool -> Room -> Model -> List (Svg Msg)
viewRoomRays isSelected room model =
    viewRoomOccs
        { roomMapper = Room.rays
        , dynamicAttributes = if isSelected then selectedAttributes room model else normalAttributes room model
        , staticAttributes = rayStyles
        , objectMapper = List.map (worldLineToScreenLine model >> (if isSelected then selectedRayTransformations room model else identity))
        , viewMapper = (\attrs lines -> List.map (Geometry.Svg.lineSegment2d attrs) lines)
        }
        room

viewRoomShadows : Bool -> Room -> Model -> List (Svg Msg)
viewRoomShadows isSelected room model =
    viewRoomOccs
        { roomMapper = Room.shadows
        , dynamicAttributes = if isSelected then selectedAttributes room model else normalAttributes room model
        , staticAttributes = shadowStyles
        , objectMapper = Map.Shadow.polygon >> worldPolyToScreenPoly model >> (if isSelected then selectedShadowTransformations room model else identity)
        , viewMapper = blackbird List.singleton Geometry.Svg.polygon2d 
        }
        room

viewRoomLights : Bool -> Room -> Model -> List (Svg Msg)
viewRoomLights isSelected room model =
    viewRoomOccs
        { roomMapper = Room.lights
        , dynamicAttributes = if isSelected then selectedAttributes room model else normalAttributes room model
        , staticAttributes = lightStyles
        , objectMapper = Map.Light.point >> worldPointToScreenPoint model >> cardinal Circle2d.atPoint (Pixels.pixels 10) >> (if isSelected then selectedLightTransformations room model else identity)
        , viewMapper = blackbird List.singleton Geometry.Svg.circle2d
        }
        room

viewRoomObjects : Bool -> Room -> Model -> List (Svg Msg)
viewRoomObjects isSelected room model =
    viewRoomOccs
        { roomMapper = Room.objects
        , dynamicAttributes = if isSelected then selectedAttributes room model else normalAttributes room model
        , staticAttributes = wallStyles
        , objectMapper = Map.Object.polygon >> worldPolyToScreenPoly model >> (if isSelected then selectedObjectTransformations room model else identity)
        , viewMapper = blackbird List.singleton Geometry.Svg.polygon2d
        }
        room

viewObjects : Room -> Model -> List (Svg Msg)
viewObjects = viewRoomOccupants Room.objects viewObject

viewShadows : Room -> Model -> Dict String (List (Svg.Attribute Msg)) -> List (Svg Msg)
viewShadows = viewRoomDataSet Room.shadows viewShadow

viewLights : Room -> Model -> List (Svg Msg)
viewLights = viewRoomOccupants Room.lights viewLight

objectViewSorter : Object -> Object -> Order
objectViewSorter obj1 obj2 =
    case ( obj1, obj2 ) of
        ( Light _, Light _ ) ->
            EQ

        ( Shadow _ _, Shadow _ _) ->
            EQ

        ( Wall _, Wall _ ) ->
            EQ

        ( Shadow _ _, _ ) ->
            LT

        ( _, Shadow _ _) ->
            GT

        ( Light _, Wall _ ) ->
            LT

        ( Wall _, Light _ ) ->
            GT


viewObject : Model -> List (Svg.Attribute Msg) -> Map.Object.Object -> List (Svg Msg)
viewObject model attrs obj =
    let
        ats = attrs ++ wallStyles
        object = (Map.Object.polygon obj |> worldPolyToScreenPoly model) 
    in
    [ Geometry.Svg.polygon2d ats object ]

viewShadow : Model -> List (Svg.Attribute Msg) -> Map.Shadow.Shadow -> List (Svg Msg)
viewShadow model attrs obj =
    [ Geometry.Svg.polygon2d (attrs ++ shadowStyles) (Map.Shadow.polygon obj |> worldPolyToScreenPoly model) ]

viewLight : Model -> List (Svg.Attribute Msg) -> Map.Light.Light -> List (Svg Msg)
viewLight model attrs obj =
    [ Geometry.Svg.circle2d (attrs ++ lightStyles) (Circle2d.atPoint (Map.Light.point obj |> worldPointToScreenPoint model) (Pixels.pixels 10)) ]

shadowStyles : List (Svg.Attribute Msg)
shadowStyles =
    [ Gats.class "shadow" ]


lightStyles : List (Svg.Attribute Msg)
lightStyles =
    [ Gats.class "light" ]


wallStyles : List (Svg.Attribute Msg)
wallStyles =
    [ Gats.class "wall" ]

screenPixelsToWorldMeters : (C.Frame -> c1 -> c2) -> (Quantity.Quantity Float (Quantity.Rate Length.Meters Pixels.Pixels) -> c2 -> c3) -> Model -> c1 -> c3
screenPixelsToWorldMeters placement conversion model =
    placement model.frame
    >> conversion (Quantity.rate (Quantity.divideBy model.scale Length.centimeter) Pixels.pixel)
    
worldMetersToScreenPixels : (C.Frame -> c2 -> c1) -> (Quantity.Quantity Float (Quantity.Rate Length.Meters Pixels.Pixels) -> c3 -> c2) -> Model -> c3 -> c1
worldMetersToScreenPixels placement conversion model =
    conversion (Quantity.rate (Quantity.divideBy model.scale Length.centimeter) Pixels.pixel)
    >> placement model.frame
    
worldPointToScreenPoint = worldMetersToScreenPixels Point2d.relativeTo Point2d.at_
screenPointToWorldPoint = screenPixelsToWorldMeters Point2d.placeIn Point2d.at

worldVectorToScreenVector = worldMetersToScreenPixels Vector2d.relativeTo Vector2d.at_
screenVectorToWorldVector = screenPixelsToWorldMeters Vector2d.placeIn Vector2d.at

worldPolyToScreenPoly = worldMetersToScreenPixels Polygon2d.relativeTo Polygon2d.at_
screenPolyToWorldPoly = screenPixelsToWorldMeters Polygon2d.placeIn Polygon2d.at

worldRectToScreenRect = worldMetersToScreenPixels Rectangle2d.relativeTo Rectangle2d.at_

worldLineToScreenLine = worldMetersToScreenPixels LineSegment2d.relativeTo LineSegment2d.at_

main : Program { width : Int, height : Int } Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
