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
import Map.Room exposing (Room)
import Map.Level exposing (Level)
import Map.Shadow exposing (Shadow)

type Msg
    = Jump
    | NoOp

type alias Model =
    { player : Player
    , level : Level
    , room : Room
    }

type alias ShadowId = String

type alias PlayerPosition = ShadowId
    --= InTheLight C.Point
    --| InTheShade ShadowId 

type alias Player =
    { position : PlayerPosition
    , direction : Direction
    , health : Float
    }

playerShadowIndex : Model -> Maybe ShadowId
playerShadowIndex {player, map} =
    Just player.position
    {-
    case player.position of
        InTheShade id -> Just id
        _ -> Nothing
    -}

nextShadowIndex : Model -> Maybe ShadowId
nextShadowIndex {player, map} =
    --for now just increments shadowId
    if player.position >= (Array.length map - 1) then
        Just 0
    else
        Just (player.position + 1)
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
playerWorldPoint {player, map} =
    Array.get player.position map
    |> Maybe.map .polygon
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

init : Level -> Room -> Model
init level room =
    { player = 
        { position = 0
        , direction = NoDirection
        , health = 100.0
        }
    , level = level
    , room = level.initial |> Tuple.second
    }


setMap : Level -> Model -> Model
setMap level model =
    { model | level = level }

shadows : Model -> List Shadow
shadows {room} =
    Dict.values (Map.Room.shadows room)
