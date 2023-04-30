module Constants exposing (..)

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
import Map.World as World
import LineSegment2d exposing (LineSegment2d)
import Circle2d exposing (Circle2d)

type alias Elapsed = Duration
type alias Time = Duration

type alias WorldCoordinates = World.WorldCoordinates

type alias Point = World.Point
type alias Polygon = World.Polygon
type alias Direction = World.Direction
type alias Rect = World.Rect

type ScreenCoordinates = ScreenCoordinates

type alias ScreenLength = Quantity Float Pixels
type alias ScreenPoint = Point2d Pixels ScreenCoordinates
type alias ScreenPolygon = Polygon2d Pixels ScreenCoordinates
type alias ScreenDirection = KArrows.Direction
type alias ScreenRect = Rectangle2d Pixels ScreenCoordinates
type alias ScreenVector = Vector2d Pixels ScreenCoordinates
type alias ScreenBox = BoundingBox2d Pixels ScreenCoordinates
type alias ScreenLine = LineSegment2d Pixels ScreenCoordinates
type alias ScreenCircle = Circle2d Pixels ScreenCoordinates

type alias Frame = Frame2d Pixels WorldCoordinates { defines : ScreenCoordinates }

meterToPixelConversion : Float -> Quantity Float (Rate Pixels Meters)
meterToPixelConversion scaleFactor =
    Quantity.multiplyBy scaleFactor (Length.centimeters 100)
    |> cardinal Quantity.per Pixels.pixel

graphicDirectionOf : ScreenDirection -> Direction
graphicDirectionOf dir =
    case dir of
        KArrows.North -> Direction2d.negativeY
        KArrows.South -> Direction2d.negativeY
        KArrows.East -> Direction2d.negativeY
        KArrows.West -> Direction2d.negativeY
        KArrows.NorthWest -> Direction2d.negativeY
        KArrows.NorthEast -> Direction2d.negativeY
        KArrows.SouthWest -> Direction2d.negativeY
        KArrows.SouthEast -> Direction2d.negativeY
        KArrows.NoDirection -> Direction2d.negativeY

{-
screenDirectionOf : Direction -> ScreenDirection
screenDirectionOf dir =
    case dir of
        KArrows.North -> Direction2d.negativeY
        KArrows.South -> Direction2d.negativeY
        KArrows.East -> Direction2d.negativeY
        KArrows.West -> Direction2d.negativeY
        KArrows.NorthWest -> Direction2d.negativeY
        KArrows.NorthEast -> Direction2d.negativeY
        KArrows.SouthWest -> Direction2d.negativeY
        KArrows.SouthEast -> Direction2d.negativeY
        KArrows.NoDirection -> Direction2d.negativeY
    -}
