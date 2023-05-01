module Map.Entity exposing (..)

import Map.Shadow exposing (Shadow)
import Map.Light exposing (Light)
import Map.Object exposing (Object)
import Map.World as World

type Entity
    = EShadow Shadow
    | ELight Light
    | EObject Object

object : Entity -> Maybe Object
object e = 
    case e of 
        EObject o -> Just o
        _ -> Nothing

light : Entity -> Maybe Light
light e = 
    case e of 
        ELight o -> Just o
        _ -> Nothing

shadow : Entity -> Maybe Shadow
shadow e = 
    case e of 
        EShadow o -> Just o
        _ -> Nothing

lightか : Entity -> Bool
lightか e =
    case e of 
        ELight _ -> True
        _ -> False
        
objectか : Entity -> Bool
objectか e =
    case e of 
        EObject _ -> True
        _ -> False

shadowか : Entity -> Bool
shadowか e =
    case e of 
        EShadow _ -> True
        _ -> False

translateBy : World.Vector -> Entity -> Entity
translateBy vec e =
    case e of
        EShadow s -> Map.Shadow.translateBy vec s |> EShadow
        EObject o -> Map.Object.translateBy vec o |> EObject
        ELight l -> Map.Light.translateBy vec l |> ELight
