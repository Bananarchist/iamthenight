port module Msg exposing (Msg(..), animationFrameSubscription, resizeSubscription, levelListener)

import Browser.Events exposing (onAnimationFrame, onResize)
import Constants as C
import Keyboard
import Time
import Duration
import Dict exposing (Dict)
import Map.Room exposing (Room)
import Map.Decode
import Result.Extra
import Map.Level exposing (Level)

type Msg 
    = KeyMsg Keyboard.Msg
    | Frame C.Time
    | Resize Int Int
    | LevelLoadingError String
    | LevelLoaded Level
    | SelectLevel Level

port levelLoaded : (String -> msg) -> Sub msg

levelListener : Sub Msg
levelListener = levelLoaded (Map.Decode.decode >> Result.mapError LevelLoadingError >> Result.map LevelLoaded >> Result.Extra.merge)

animationFrameSubscription : Sub Msg
animationFrameSubscription = onAnimationFrame (Time.posixToMillis >> toFloat >> Duration.milliseconds >> Frame)

resizeSubscription : Sub Msg
resizeSubscription = onResize Resize
