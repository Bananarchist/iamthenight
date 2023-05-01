module Logic exposing (..)

import Array exposing (Array)
import Constants as C
import Polygon2d
import Point2d
import Keyboard exposing (Key)
import Msg
import Aviary.Birds exposing (cardinal)
import Keyboard.Arrows exposing (Direction(..))
import Dict exposing (Dict)
import Map.Room as Room exposing (Room)
import Map.Level exposing (Level)
import Map.Shadow exposing (Shadow)
import Map.Entity
import Helpers exposing (maybeTuple)

type Msg
    = Jump
    | NoOp

type alias Model =
    { player : Player
    , level : Level
    , room : Room
    }

type alias ShadowId = String

type alias PlayerPosition = Room.EntityId

type alias Player =
    { position : PlayerPosition
    , direction : Direction
    , health : Float
    }

playerShadow : Model -> Maybe Shadow
playerShadow model =
    Room.selectEntities [model.player.position] model.room
    |> List.head
    |> Maybe.andThen Map.Entity.shadow

playerShadowIndex : Model -> Maybe ShadowId
playerShadowIndex {player, room} =
    Just "somethin to compile"

nextShadowIndex : Model -> Maybe ShadowId
nextShadowIndex {player, room} =
    --for now just increments shadowId
    Just "something tog compile"
    {-
    if player.position >= (Array.length map - 1) then
        Just 0
    else
        Just (player.position + 1)
    -}
    {-
    case player.position of
        InTheShade id ->
            if id >= (Array.length map - 1) then
                Just 0
            else
                Just (id + 1)
        _ -> Nothing
    -}

playerWorldPoint : Model -> C.Point
playerWorldPoint {player, room} =
    Room.selectEntities [player.position] room
    |> List.head
    |> Maybe.andThen Map.Entity.shadow
    |> Maybe.map Map.Shadow.polygon
    |> Maybe.andThen Polygon2d.centroid
    |> Maybe.withDefault Point2d.origin
    {-
    case player.position of
        InTheLight point -> point
        InTheShade id ->
            Array.get id map
            |> Maybe.andThen Polygon2d.centroid
            |> Maybe.withDefault Point2d.origin
    -}

update : Msg -> Model -> (Model, Cmd Msg.Msg)
update msg model = 
    case msg of
        Jump ->
            let
                player = model.player
            in
            { model | player = { player | position = nextShadowIndex model |> {-Maybe.map InTheShade |> -} Maybe.withDefault player.position }}
            |> cardinal Tuple.pair Cmd.none
        NoOp ->
            (model, Cmd.none)

subscriptions = always Sub.none

init : Level -> Maybe Model
init level =
    Dict.get level.entry level.rooms
    |> Maybe.map(\r ->
        { player = 
            { position = Room.entryId r
            , direction = NoDirection
            , health = 100.0
            }
        , level = level
        , room = r
        }
    )


setMap : Level -> Model -> Model
setMap level model =
    { model | level = level }

shadows : Model -> List Shadow
shadows {room} =
    Dict.values (Room.shadows room)

