module Map.Level exposing (..)

import Dict exposing (Dict)
import Map.Room exposing (Room)

type alias RoomId = String

type alias Level =
    { name : String
    , initial : (RoomId, Room)
    , room_to_room : Dict RoomId (List RoomId, List RoomId)
    , rooms : Dict RoomId Room
    }
