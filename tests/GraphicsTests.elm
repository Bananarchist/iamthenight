module GraphicsTests exposing (suite)


import Test exposing (Test, describe, test, fuzz)
import Expect
import Fuzz
import Graphics
import Constants as C
import Polygon2d
import Point2d
import Direction2d
import Keyboard.Arrows exposing (Direction(..))
import Helpers exposing (uncurry)
import Pixels exposing (Pixels)

fuzzScreenDirection : Fuzz.Fuzzer C.ScreenDirection
fuzzScreenDirection =
  Fuzz.oneOf
    [ Fuzz.constant North
    , Fuzz.constant South
    , Fuzz.constant East
    , Fuzz.constant West
    , Fuzz.constant NorthWest
    , Fuzz.constant NorthEast
    , Fuzz.constant SouthWest
    , Fuzz.constant SouthEast
    , Fuzz.constant NoDirection
    ]

suite : Test.Test
suite =
  [ fuzz fuzzScreenDirection "for normal ass square" <|
    \dir ->
      let
          expectedPoint = 
            (case dir of
              North -> (0, -10)
              South -> (0, 10)
              East -> (10, 0)
              West -> (-10, 0)
              NorthWest -> (-10, -10)
              NorthEast -> (10, -10)
              SouthWest -> (-10, 10)
              SouthEast -> (10, 10)
              NoDirection -> (0, 0))
            |> uncurry Point2d.pixels
          poly =
            [ (-10, -10), (10, -10), (10, 10), (-10, 10) ]
            |> List.map (uncurry Point2d.pixels)
            |> Polygon2d.singleLoop
          guess = 
            Graphics.pointAtEdgeOfPolygon poly dir
            |> Maybe.withDefault Point2d.origin
      in
      Point2d.equalWithin (Pixels.pixels 1) expectedPoint guess
      |> Expect.equal True
      |> Expect.onFail (String.join "\n" ["Unequal", Debug.toString expectedPoint, Debug.toString guess, Debug.toString (Polygon2d.boundingBox poly)])
  , fuzz fuzzScreenDirection "for dumb triangle" <|
    \dir ->
      let
          expectedPoint = 
            (case dir of
              North -> (0, -10)
              South -> (0, 10)
              East -> (5, 0)
              West -> (-5, 0)
              NorthWest -> (-3.3, -3.3)
              NorthEast -> (3.3, -3.3)
              SouthWest -> (-10, 10)
              SouthEast -> (10, 10)
              NoDirection -> (0, 0))
            |> uncurry Point2d.pixels
          poly =
            [ (0, -10), (10, 10), (-10, 10)] |> List.map (uncurry Point2d.pixels) |> Polygon2d.singleLoop
          guess = 
            Graphics.pointAtEdgeOfPolygon poly dir
            |> Maybe.withDefault Point2d.origin
      in
      Point2d.equalWithin (Pixels.pixels 1) expectedPoint guess
      |> Expect.equal True
      |> Expect.onFail (String.join "\n" ["Unequal", Debug.toString expectedPoint, Debug.toString guess, Debug.toString (Polygon2d.boundingBox poly)])
  , fuzz fuzzScreenDirection "for silly quad" <|
    \dir ->
      let
          expectedPoint = 
            (case dir of
              North -> (1.5, -1.6)
              South -> (1, 6)
              East -> (7.5, 2.5)
              West -> (-5, 3)
              NorthWest -> (-5, -1)
              NorthEast -> (8, -2)
              SouthWest -> (-5, 6)
              SouthEast -> (7, 6)
              NoDirection -> (1.5, 2))
            |> uncurry Point2d.pixels
          poly =
            [ (-5, -1), (8, -2), (7, 6), (-5, 6)] |> List.map (uncurry Point2d.pixels) |> Polygon2d.singleLoop
          guess = 
            Graphics.pointAtEdgeOfPolygon poly dir
            |> Maybe.withDefault Point2d.origin
      in
      Point2d.equalWithin (Pixels.pixels 1) expectedPoint guess
      |> Expect.equal True
      |> Expect.onFail (String.join "\n" ["Unequal", Debug.toString expectedPoint, Debug.toString guess, Debug.toString (Polygon2d.boundingBox poly)])
  ]
  |> describe "Getting point at edge of polygon"
