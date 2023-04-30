module Helpers exposing (..)

import Dict exposing (Dict)
import Keyboard.Arrows exposing (Direction(..))
import Aviary.Birds exposing (kestrel)
import Aviary.Birds exposing (cardinal)
import Aviary.Birds exposing (blackbird)
import Vector2d
import Quantity
import Point2d exposing (Point2d)
import BoundingBox2d

duple : a -> (a, a)
duple a = (a, a)

uncurry : (a -> b -> c) -> (a, b) -> c
uncurry fn (a, b) = fn a b

mapTuple : (a -> b) -> (a, a) -> (b, b)
mapTuple fn (a, b) = (fn a, fn b)

sortTuple : (a -> a -> Order) -> (a, a) -> (a, a)
sortTuple comparator (a, b) =
    case comparator a b of
        LT -> (a, b)
        GT -> (b, a)
        EQ -> (a, b)

tupleList : (a, a) -> List a
tupleList (a, b) = [a, b]

elput : b -> a -> (a, b)
elput = cardinal Tuple.pair 

bool : a -> a -> Bool -> a
bool trueValue falseValue testValue =
    if testValue then trueValue else falseValue

greater : (b -> b -> Order) -> (a -> b) -> a -> a -> a
greater comparator mapper val1 val2 =
    case comparator (mapper val1) (mapper val2) of
        LT -> val2
        _ -> val1

lesser : (b -> b -> Order) -> (a -> b) -> a -> a -> a
lesser comparator mapper val1 val2 =
    case comparator (mapper val1) (mapper val2) of
        GT -> val2
        _ -> val1

isCardinalか : Direction -> Bool
isCardinalか dir =
    case dir of 
        North -> True
        East -> True
        South -> True
        West -> True
        _ -> False

maybeTuple : (Maybe a, Maybe b) -> Maybe (a, b)
maybeTuple =
    uncurry (Maybe.map2 Tuple.pair)

bothか : (a -> Bool) -> a -> a -> Bool
bothか predicate a =
    Tuple.pair a
    >> mapTuple predicate
    >> uncurry (&&)

eitherか : (a -> Bool) -> a -> a -> Bool
eitherか predicate a =
    Tuple.pair a
    >> mapTuple predicate
    >> uncurry (||)

selectDictKeys : List comparable -> Dict comparable v -> Dict comparable v 
selectDictKeys keys =
    Dict.filter (cardinal List.member keys >> kestrel)

getDictValues : List comparable -> Dict comparable v -> List v
getDictValues =
    blackbird Dict.values selectDictKeys

nearestPointTo : Point2d x y -> Point2d x y -> List (Point2d x y) -> Point2d x y
nearestPointTo target tPoint tPoints =
    if List.isEmpty tPoints then tPoint
    else
        let
            lengths = List.map (duple >> Tuple.mapFirst (Vector2d.from target >> Vector2d.length)) (tPoint :: tPoints) 
        in
        Quantity.minimumBy Tuple.first lengths
        |> Maybe.map Tuple.second
        |> Maybe.withDefault tPoint



pointBetweenか : Point2d x y -> Point2d x y -> Point2d x y -> Bool
pointBetweenか p1 p2 test =
    BoundingBox2d.from p1 p2
    |> BoundingBox2d.contains test


{-| Tests if all predicates applied to a value return True, returning
False as soon as it is encountered and ignoring remaining tests
-}
allPredicates : List (a -> Bool) -> a -> Bool
allPredicates fns val =
    let
        -- define iterator for short circuiting
        listIter predicates param last =
            case ( predicates, last ) of
                ( _, False ) ->
                    False

                ( p :: ps, True ) ->
                    listIter ps param (p param)

                ( [], _ ) ->
                    last
    in
    listIter fns val True


{-| Tests all predicates to assure at least one returns True
-}
anyPredicates : List (a -> Bool) -> a -> Bool
anyPredicates fns val =
    let
        listIter predicates param last =
            case ( predicates, last) of
                (_, True) -> True
                ( p :: ps, False) ->
                    listIter ps param (p param)
                ( [], _) -> 
                    last
    in
    listIter fns val False
    --List.foldl ((|>) val >> (||)) False fns

