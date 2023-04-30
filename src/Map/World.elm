module Map.World exposing (..)

import Keyboard.Arrows as KArrows
import Direction2d exposing (Direction2d)
import Duration exposing (Duration, seconds)
import Frame2d exposing (Frame2d)
import Length exposing (Meters)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Pixels exposing (Pixels)
import Quantity exposing (Quantity, Rate)
import Aviary.Birds exposing (cardinal)
import Keyboard.Arrows exposing (Direction)
import Rectangle2d exposing (Rectangle2d)
import Vector2d exposing (Vector2d)
import BoundingBox2d exposing (BoundingBox2d)
import LineSegment2d exposing (LineSegment2d)

type WorldCoordinates = WorldCoordinates

type alias Point = Point2d Meters WorldCoordinates
type alias Polygon = Polygon2d Meters WorldCoordinates
type alias Direction = Direction2d WorldCoordinates
type alias Elapsed = Duration
type alias Time = Duration
type alias Rect = Rectangle2d Meters WorldCoordinates
type alias BoundingBox = BoundingBox2d Meters WorldCoordinates
type alias Vector = Vector2d Meters WorldCoordinates
type alias Line = LineSegment2d Meters WorldCoordinates
