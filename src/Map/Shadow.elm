module Map.Shadow exposing (..)

import Map.World as World

type Shadow = Shadow World.Polygon

new : World.Polygon -> Shadow
new = Shadow

polygon : Shadow -> World.Polygon
polygon (Shadow poly) = poly
