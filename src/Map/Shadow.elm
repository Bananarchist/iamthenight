module Map.Shadow exposing (..)

import Map.World as World
import Polygon2d

type Shadow = Shadow World.Polygon

new : World.Polygon -> Shadow
new = Shadow

polygon : Shadow -> World.Polygon
polygon (Shadow poly) = poly

translateBy : World.Vector -> Shadow -> Shadow
translateBy vec (Shadow poly) =
    Polygon2d.translateBy vec poly |> Shadow

