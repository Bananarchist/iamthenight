module Graphics exposing (..)
import BoundingBox2d
import Length
import Polygon2d
import Keyboard.Arrows exposing (Direction(..))

import Animator
import Direction2d
import Logic
import Keyboard exposing (Key)
import Keyboard.Arrows exposing (Direction(..))
import Array exposing (Array)
import Constants as C
import Html exposing (Html)
import Msg
import Svg exposing (Svg)
import Svg.Attributes as Gats
import Frame2d
import Polygon2d
import Point2d
import Helpers exposing (uncurry, mapTuple)
import Aviary.Birds exposing (cardinal)
import Pixels
import Aviary.Birds exposing (kestrel)
import Aviary.Birds exposing (dove)
import Helpers exposing (duple)
import BoundingBox2d
import Quantity exposing (Quantity)
import Pixels exposing (Pixels)
import LineSegment2d exposing (LineSegment2d)
import Axis2d
import Helpers exposing (lesser)
import Helpers exposing (greater)
import Helpers exposing (isCardinalか)
import Parameter1d
import Helpers exposing (maybeTuple)
import Vector2d
import Angle
import Duration exposing (Duration)
import Time
import Dict
import Map.Shadow

type Msg
    = Resize Int Int
    | Frame Duration
    | Lean Direction
    | Leap
    | NoOp

type alias Player =
    { direction : Direction
    , position : C.ScreenPoint
    }

type alias Model =
    { frame : C.Frame
    , player : Animator.Timeline Player
    , shadows : Array C.ScreenPolygon
    , width : Quantity Float Pixels
    , height : Quantity Float Pixels
    , scale : Float
    }

init : Int -> Int -> Logic.Model -> Model
init w h logic = 
    let
        scale = 1 --min ((toFloat w) / 6000)  ((toFloat h) / 4000)
        frame =
            Frame2d.atPoint Point2d.origin --(Point2d.pixels (w // 2 |> toFloat |> (*) scale |> negate) (h // 2 |> toFloat |> (*) scale |> negate)) --|> Frame2d.reverseY
        subModel = { scale = scale, frame = frame }
        shadows = 
            logic.shadows |> Dict.map (\_ -> Map.Shadow.polygon |> polygonInFrame subModel)
    in
    { frame = frame
    , player = 
        { direction = Keyboard.Arrows.NoDirection
        , position = 
            Logic.playerShadowIndex logic
            |> Maybe.andThen (cardinal Array.get shadows)
            |> Maybe.andThen (cardinal pointAtEdgeOfPolygon Keyboard.Arrows.NoDirection)
            |> Maybe.withDefault (Logic.playerWorldPoint logic |> pointInFrame subModel)
        }
        |> Animator.init
    , shadows = shadows
    , width = w |> toFloat |> Pixels.float
    , height = h |> toFloat |> Pixels.float
    , scale = scale
    }
    
update : Msg -> Logic.Model -> List Key -> Model -> Model
update msg logic keys og =
    case msg of
        Resize w h ->
            { og 
            | width = w |> toFloat |> Pixels.float
            , height = h |> toFloat |> Pixels.float
            , scale = min (3 / (toFloat w)) (2 / (toFloat h))
            }
        Lean direction ->
            let
                p = Animator.current og.player
            in
            { og
            | player =
                { p 
                | position =
                    Logic.playerShadowIndex logic
                    |> Maybe.andThen (cardinal Array.get og.shadows)
                    |> Maybe.andThen (cardinal pointAtEdgeOfPolygon direction)
                    |> Maybe.withDefault p.position
                }
                |> (cardinal (Animator.go Animator.slowly) og.player)
            }
        Leap ->
            og
        Frame tick ->
            Animator.update (tick |> Duration.inMilliseconds |> floor |> Time.millisToPosix) animator og
        NoOp ->
            og


mapPlayer : (Animator.Timeline Player -> Animator.Timeline Player) -> Model -> Model
mapPlayer fn model =
    { model | player = fn model.player } 

animator : Animator.Animator Model
animator =
    Animator.animator
    |> Animator.watchingWith .player (\np -> mapPlayer (kestrel np))
        (\{direction, position} ->
            direction /= NoDirection)

subscriptions : Model -> Sub Msg.Msg
subscriptions model =
    Sub.batch 
        [ Msg.resizeSubscription 
        , Animator.toSubscription (Time.posixToMillis >> toFloat >> Duration.milliseconds >> Msg.Frame) model animator 
        ]

view : Model -> Html Msg.Msg
view model = 
    model.shadows
    |> Array.toList
    |> List.concatMap shadowPolygon
    |> cardinal (++) 
        (model.player |> playerSprite)
    |> (++) defs
    |> Svg.svg 
        [ viewBox model
        , Gats.width (model.width |> Pixels.inPixels |> String.fromFloat)
        , Gats.height (model.height |> Pixels.inPixels |> String.fromFloat)
        ]
    |> dove Html.main_ [] List.singleton


polygonInFrame : { a | frame : C.Frame, scale : Float } -> C.Polygon -> C.ScreenPolygon
polygonInFrame {scale, frame} = Polygon2d.at (C.meterToPixelConversion scale) >> Polygon2d.relativeTo frame

pointInFrame : { a | frame : C.Frame, scale : Float } -> C.Point -> C.ScreenPoint
pointInFrame {scale, frame} = Point2d.at (C.meterToPixelConversion scale) >> Point2d.relativeTo frame

{-
Take poly
Turn into line segments
Get xfn and yfn (Linesegment.midpoint or endpoints >> xcom/ycom >> max/min)
grab the necessary point
-}
newestmostpointAtEdgeOfPolygon : C.ScreenPolygon -> C.ScreenDirection -> Maybe C.ScreenPoint
newestmostpointAtEdgeOfPolygon polygon direction =
    let
        mbBB = Polygon2d.boundingBox polygon
        mbCenter =  mbBB |> Maybe.map BoundingBox2d.centerPoint
    in
    Maybe.andThen (\bb ->
        let
            center = BoundingBox2d.centerPoint bb
            (x, y) = Point2d.coordinates center
            {maxX, minX, maxY, minY} = BoundingBox2d.extrema bb
            targetPoint = 
                ( case direction of
                    NoDirection -> (x, y)
                    North -> (x, minY)
                    South -> (x, maxY)
                    West -> (minX, y)
                    East -> (maxX, y)
                    NorthWest -> (minX, minY)
                    NorthEast -> (maxX, minY)
                    SouthWest -> (minX, maxY)
                    SouthEast -> (maxX, maxY)
                ) 
                |> uncurry Point2d.xy
            targetAngle = 
                ( case direction of
                    NoDirection -> 0
                    North -> 90
                    South -> -90
                    West -> 180
                    East -> 0
                    NorthWest -> -135
                    NorthEast -> 45
                    SouthWest -> 135
                    SouthEast -> -45
                )
                |> Direction2d.degrees

            edgeSteps edge =
                LineSegment2d.length edge
                |> Quantity.multiplyBy 2
                |> Quantity.floor
                |> Pixels.inPixels
        in
        Polygon2d.edges polygon
        |> List.concatMap 
            ( duple 
            >> Tuple.mapFirst edgeSteps 
            >> (\(steps, edge) -> Parameter1d.steps steps (LineSegment2d.interpolate edge))
            )
        |> (::) center
        |> List.map (duple >> Tuple.mapBoth Just (Direction2d.from center >> Maybe.map (Direction2d.angleFrom targetAngle)) >> maybeTuple)
        |> List.filterMap identity
        --|> List.map (duple >> Tuple.mapSecond (Point2d.distanceFrom targetPoint))
        |> Quantity.minimumBy Tuple.second
        |> Maybe.map Tuple.first
    ) mbBB



pointAtEdgeOfPolygon : C.ScreenPolygon -> C.ScreenDirection -> Maybe C.ScreenPoint
pointAtEdgeOfPolygon polygon direction =
    let
        mbBB = Polygon2d.boundingBox polygon
        mbCenter =  mbBB |> Maybe.map BoundingBox2d.centerPoint
    in
    if direction == NoDirection then
        mbCenter
    else
        let
            axisThroughOrigin = (\d -> (Maybe.map (Axis2d.withDirection d) mbCenter))
            axis = 
                case direction of
                    North -> Just Axis2d.y --axisThroughOrigin (Direction2d.degrees -90)
                    South -> Just Axis2d.y --axisThroughOrigin (Direction2d.degrees -90)
                    East -> Just Axis2d.x --axisThroughOrigin (Direction2d.degrees 180)
                    West -> Just Axis2d.x --axisThroughOrigin (Direction2d.degrees 180)
                    NorthEast -> axisThroughOrigin (Direction2d.degrees 45)
                    NorthWest -> axisThroughOrigin (Direction2d.degrees 135)
                    SouthEast -> axisThroughOrigin (Direction2d.degrees -45)
                    SouthWest -> axisThroughOrigin (Direction2d.degrees 135)
                    NoDirection -> axisThroughOrigin (Direction2d.degrees 135)
            end =
                case direction of
                    North -> duple >> Tuple.mapBoth BoundingBox2d.centerPoint (duple >> Tuple.mapBoth (BoundingBox2d.centerPoint >> Point2d.xCoordinate) BoundingBox2d.minY >> uncurry Point2d.xy) >> uncurry Point2d.distanceFrom
                    South -> duple >> Tuple.mapBoth BoundingBox2d.centerPoint (duple >> Tuple.mapBoth (BoundingBox2d.centerPoint >> Point2d.xCoordinate) BoundingBox2d.maxY >> uncurry Point2d.xy) >> uncurry Point2d.distanceFrom
                    East -> duple >> Tuple.mapBoth BoundingBox2d.centerPoint (duple >> Tuple.mapBoth BoundingBox2d.minX (BoundingBox2d.centerPoint >> Point2d.yCoordinate) >> uncurry Point2d.xy) >> uncurry Point2d.distanceFrom
                    West -> duple >> Tuple.mapBoth BoundingBox2d.centerPoint (duple >> Tuple.mapBoth BoundingBox2d.maxX (BoundingBox2d.centerPoint >> Point2d.yCoordinate) >> uncurry Point2d.xy) >> uncurry Point2d.distanceFrom
                    NorthWest -> duple >> Tuple.mapBoth BoundingBox2d.centerPoint (BoundingBox2d.extrema >> \{maxX, minY} -> Point2d.xy maxX minY) >> uncurry Point2d.distanceFrom
                    NorthEast -> duple >> Tuple.mapBoth BoundingBox2d.centerPoint (BoundingBox2d.extrema >> \{maxX, minY} -> Point2d.xy maxX minY) >> uncurry Point2d.distanceFrom
                    SouthWest -> duple >> Tuple.mapBoth BoundingBox2d.centerPoint (BoundingBox2d.extrema >> \{maxX, minY} -> Point2d.xy maxX minY) >> uncurry Point2d.distanceFrom
                    SouthEast -> duple >> Tuple.mapBoth BoundingBox2d.centerPoint (BoundingBox2d.extrema >> \{maxX, minY} -> Point2d.xy maxX minY) >> uncurry Point2d.distanceFrom
                    NoDirection -> kestrel (Pixels.pixels 0)
            angle =
                ( case direction of
                    North -> -90
                    South -> 90
                    East -> 0
                    West -> 180
                    NorthEast -> -45
                    NorthWest -> -135
                    SouthEast -> 45
                    SouthWest -> 135
                    NoDirection -> 0
                )
                |> Angle.degrees

            vector = Vector2d.rTheta (Polygon2d.perimeter polygon) angle
            testLine = Maybe.map (cardinal LineSegment2d.fromPointAndVector vector) mbCenter
            --testLine = Maybe.map2 (\a b -> LineSegment2d.along a (Pixels.pixels 0) (Polygon2d.perimeter polygon)) axis mbBB
        in
        Polygon2d.edges polygon
        |> List.filterMap (\v -> Maybe.andThen (LineSegment2d.intersectionPoint v) testLine)
        |> List.head


newpointAtEdgeOfPolygon : C.ScreenPolygon -> C.ScreenDirection -> Maybe C.ScreenPoint
newpointAtEdgeOfPolygon polygon direction =
    if direction == NoDirection then
        Polygon2d.boundingBox polygon |> Maybe.map BoundingBox2d.centerPoint
    else
        let
            pointMapper = 
                duple 
                >> Tuple.mapBoth LineSegment2d.midpoint (LineSegment2d.endpoints >> (\(a, b) -> [a, b]))
                >> uncurry (::)
            axisThroughOrigin = Axis2d.through Point2d.origin
            axis = 
                case direction of
                    North -> axisThroughOrigin (Direction2d.degrees -90)
                    South -> axisThroughOrigin (Direction2d.degrees 90)
                    East -> axisThroughOrigin (Direction2d.degrees 0)
                    West -> axisThroughOrigin (Direction2d.degrees 180)
                    NorthEast -> axisThroughOrigin (Direction2d.degrees -45)
                    NorthWest -> axisThroughOrigin (Direction2d.degrees -135)
                    SouthEast -> axisThroughOrigin (Direction2d.degrees 45)
                    SouthWest -> axisThroughOrigin (Direction2d.degrees 135)
                    NoDirection -> axisThroughOrigin (Direction2d.degrees 135)
                    
            pointSorter : (C.ScreenPoint -> C.ScreenPoint -> C.ScreenPoint)
            pointSorter = greater Quantity.compare (Point2d.signedDistanceAlong axis)
            {-
                case dir of 
                    North -> greater Quantity.compare (Point2d.signedDistanceAlong axis)
                    South -> greater Quantity.compare (Point2d.signedDistanceAlong axis)
                    East -> (\p1 p2 -> if Quantity.compare (Point2d.xCoordinate p1) (Point2d.xCoordinate p2) == GT then p2 else p1)
                    West -> (\p1 p2 -> if Quantity.compare (Point2d.xCoordinate p1) (Point2d.xCoordinate p2) == LT then p2 else p1)
                    NorthWest -> (\p1 p2 -> 
                        let ax = (Axis2d.through Point2d.origin (Direction2d.degrees 135))
                        in if Quantity.compare (Point2d.signedDistanceAlong ) >> uncurry Quantity.max)
                    _ -> always -}
        in
        Polygon2d.edges polygon
        |> List.concatMap pointMapper
        |> List.foldl (\el -> Maybe.map (pointSorter el)) (Polygon2d.centroid polygon)


oldpointAtEdgeOfPolygon : C.ScreenPolygon -> C.ScreenDirection -> Maybe C.ScreenPoint
oldpointAtEdgeOfPolygon polygon dir =
    let
        mbBB = Polygon2d.boundingBox polygon
        mbCenter = Maybe.map BoundingBox2d.centerPoint mbBB --Polygon2d.centroid polygon
    in
    if dir == NoDirection then
        mbCenter
    else
        let
            (xFn, yFn) =
                case dir of
                    NorthWest -> (BoundingBox2d.minX, BoundingBox2d.minY)
                    NorthEast -> (BoundingBox2d.maxX, BoundingBox2d.minY)
                    SouthWest -> (BoundingBox2d.minX, BoundingBox2d.maxY)
                    SouthEast -> (BoundingBox2d.maxX, BoundingBox2d.maxY)
                    North -> (BoundingBox2d.midX, BoundingBox2d.minY)
                    South -> (BoundingBox2d.midX, BoundingBox2d.maxY)
                    East -> (BoundingBox2d.maxX, BoundingBox2d.midY)
                    West -> (BoundingBox2d.minX, BoundingBox2d.midY)
                    NoDirection -> (BoundingBox2d.minX, BoundingBox2d.midY) -- unused
            edgePoint = 
                mbBB
                |> Maybe.map (duple >> Tuple.mapBoth xFn yFn >> uncurry Point2d.xy)
            checker edge guess =
                if Point2d.equalWithin Pixels.pixel edge guess then
                    guess |> Debug.log "Here's the guess!"
                else
                    let mp = Point2d.midpoint guess edge
                    in
                    if Polygon2d.contains mp polygon then
                        checker edge mp
                    else
                        checker mp guess

        in
        if Maybe.map (cardinal Polygon2d.contains polygon) edgePoint |> Maybe.withDefault False then
            edgePoint
        else
            Maybe.map2 checker edgePoint mbCenter

screenPointToStringCoordinatePair : C.ScreenPoint -> String
screenPointToStringCoordinatePair =
    Point2d.toTuple Pixels.inPixels
        >> Tuple.mapBoth String.fromFloat (String.fromFloat >> List.singleton) 
        >> uncurry (::) 
        >> String.join ","

coordinatePairListToPoints : List String -> List (Svg.Attribute Msg.Msg)
coordinatePairListToPoints =
    String.join " "
    >> Gats.points
    >> List.singleton


shadowPolygon : C.ScreenPolygon -> List (Svg Msg.Msg)
shadowPolygon = 
    Polygon2d.outerLoop
    >> List.map screenPointToStringCoordinatePair
    >> coordinatePairListToPoints
    >> (++) [ Gats.stroke "none", Gats.fill "black" ]
    >> cardinal Svg.polygon []
    >> List.singleton

viewBox : Model -> Svg.Attribute Msg.Msg
viewBox model = 
    let
        {x, y} = Frame2d.originPoint model.frame |> Point2d.toPixels
        (w, h) = model |> duple |> Tuple.mapBoth .width .height |> mapTuple (Pixels.inPixels >> (*) model.scale)
    in
    String.join " "
        [ String.fromFloat (x - 15)
        , String.fromFloat (y - 10)
        , String.fromFloat 30
        , String.fromFloat 20
        ]
    |> Gats.viewBox

playerSprite : Animator.Timeline Player -> List (Svg Msg.Msg)
playerSprite =
    cardinal Animator.xy (.position >> Point2d.toRecord Pixels.inPixels >> (\{x, y} -> {x = Animator.at x |> Animator.leaveLate 0.1 |> Animator.arriveEarly 0.1, y = Animator.at y |> Animator.leaveLate 0.1 |> Animator.arriveEarly 0.1}))
    >> (\{x, y} -> (x, y))
    >> Tuple.mapBoth ((+) -1 >> String.fromFloat >> Gats.x) ((+) -1 >> String.fromFloat >> (Gats.y >> List.singleton))
    >> uncurry (::)
    >> cardinal (++) [ Gats.xlinkHref "#face", Gats.width "2", Gats.height "2" ] --, Gats.width "10", Gats.height "10" ]
    >> cardinal Svg.use []
    >> List.singleton

defs = 
    [ Svg.defs []
        [ playerSvgData ]
    ]

playerSvgData =
    Svg.svg
        --[ Gats.id "face" ]
        [ Gats.viewBox "0 0 103.978 99.424"
        , Gats.id "face"
        ] 
        [ Svg.path
            [ Gats.d "M25.023 76.906 8.768 85.372M5.987 69.26c2.878-.048 5.743.357 8.601.643 3.673.444 7.361.738 11.056.904 2.559.15 5.118-.022 7.657-.336 1.727-.272 3.442-.601 5.153-.957.908-.126 1.766-.45 2.644-.696.477-.143.97-.208 1.465-.232 1.786.054-3.941-1.037-3.072-1.912-.116-.064-.218.23-.262.33a.796.796 0 0 0-.068.34c-.002.287.01.574.016.861.147 1.774.851 3.443 1.41 5.116.608 1.895 1.253 3.778 2.02 5.615.27.495.364 1.078.64 1.568 3.44 4.708 3.644 2.218 3.995.5.403-1.548 1.016-3.028 1.575-4.522a218.35 218.35 0 0 0 2.186-6.536c.494-1.368.875-2.776 1.508-4.09.09-.073.306-1.016.333-.492-8.84-4.956.248.18.306.211.077.043.178.02.256.002.79.195-4.767-3.05-2.888-1.527 1.21.696 2.402 1.425 3.63 2.088.12.064.265.055.397.086.177.041.352.088.529.132.852.21 1.707.415 2.56.623 2.492.633 4.914 1.49 7.361 2.27 1.522.45 3.006 1.011 4.509 1.52.206.201.614.09.84.214.036.02.105.122.069.102-1.108-.62-2.197-1.273-3.295-1.91.909 1.14 2.486 2.01 4.015 2.329.449.013.83.426 1.279.346.443-.186.42-.769.435-1.174.002-.817-.166-1.623-.249-2.435-.09-1.046-.098-2.097-.11-3.146a331.98 331.98 0 0 1 .002-3.156c.003-.754.01-1.508.014-2.262.006-.3-.039-.595.117-.86.076-.11.28-.058.411-.057.564-.11 1.142-.266 1.714-.38a73.172 73.172 0 0 1 4.937-.517c1.94-.112 3.876-.3 5.819-.394 1.515.001 3.019-.164 4.526-.292.947-.078 1.897-.1 2.847-.11.48-.001.96.004 1.439.009.531.044.41-.548.244-.863-.477-.79-.938-1.59-1.408-2.384-.9-1.4-1.818-2.787-2.72-4.186-.667-1.018-1.21-2.108-1.81-3.165-.071.012-.42-.867-.452-.854-.037.015-.037.072-.056.108-.056.128.04.17.15.168.293-.049.555-.464.71-.7.763-1.048 1.553-2.072 2.282-3.145 1.261-1.902 2.518-3.808 3.72-5.748 1.014-1.689 2.055-3.363 2.972-5.107.619-1.168 1.129-2.383 1.622-3.607.276-.675.522-1.36.66-2.078.52-1.328-3.412-3.359-3.487-1.868-.005.225-.005.45-.002.675.025.64.27 1.26.443 1.874.352 1.414.745 2.817 1.091 4.233.357 1.33.679 2.67 1.008 4.009.301 1.23.493 2.487.655 3.743.146 1.727.32 3.453.435 5.184.115 1.87.135 3.744.142 5.618.004 1.732.006 3.465 0 5.198-.021 1.458.027 2.917-.047 4.373-.116 1.69-.217 3.377-.431 5.057-.158 1.212-.358 2.418-.517 3.63-.011.033-.017.464-.158.46-.043-.001-.078-.04-.121-.046-.273-.038-.55-.032-.825-.039-.298-.025-.57-.205-.862-.31-.45-.09-.886-.197-1.348-.255-.991-.257-2.01-.334-3.029-.382-1.4 0-2.791-.17-4.187-.271-1.106-.087-2.215-.05-3.309-.25-.312.006-.922-.358-1.197-.055-.065.072-.1.166-.15.25-.192.685-.124 1.42-.113 2.125.066 1.431-.009 2.855-.119 4.28-.066 1.097-.14 2.184-.339 3.265-.117.665-.325 1.307-.438 1.974-.128.55-.273 1.095-.388 1.647a.84.84 0 0 1-.262.58c-.056-.002-.074-.059-.204-.072-.11-.014-.222-.007-.333-.008-.25.001-.492.039-.721-.068-.925-.299-1.861-.562-2.795-.838a46.643 46.643 0 0 0-4.375-.932c-.892-.113-1.76-.344-2.653-.45-.795-.107-1.58-.257-2.381-.33a35.687 35.687 0 0 0-2.019-.087c-.413.01-.55-.122-.814.238a6.346 6.346 0 0 0-.3.791c-.127.63-.233 1.263-.357 1.893a54.393 54.393 0 0 1-1.046 3.721c-.322 1.13-.63 2.263-.985 3.384-.169.758-.511 1.448-.81 2.157a15.51 15.51 0 0 1-.576 1.355c-.201.454-.501.85-.753 1.276-.165.346-.358.694-.63.967-.144.166-.22.16-.41.155-.284.02-.484-.097-.744-.197-1.522-.548-3.072-1.009-4.625-1.457a134.31 134.31 0 0 0-7.092-1.536c-1.675-.367-3.362-.686-5.033-1.072-.917-.156-1.796-.449-2.686-.707-.416-.103-.656-.262-.981.068-.235.423-.18.935-.18 1.403.01 1.084-.144 2.164-.298 3.235-.18.986-.157 2.008-.42 2.978-.081.201-.183.594-.25.705-.122.01-.246.263-.311.375-.007.013-.021.05-.02.037.008-.145-.018-.107-.157-.118-.082-.045-.238.038-.313-.008-.032-.02-.038-.067-.058-.1l-.128-.059c-1.353-.948 4.037 2.144 2.984 1.698-1.324-.561-2.504-1.416-3.778-2.082-.577-.302-1.408-.358-2.032-.495-2.271-.484-4.57-.824-6.85-1.268-1.986-.267-3.856-.98-5.729-1.656-1.74-.658-3.412-1.478-5.103-2.248-1.01-.51-.285-.13-1.212-.651a15.52 15.52 0 0 0-.505-.278c5.269 3.023 3.41 1.99 2.276 1.24-.657-.423-1.22-.966-1.781-1.506-.924-.863-1.78-1.79-2.615-2.738-.773-.819-1.429-1.73-2.083-2.641-.575-.848-1.053-1.754-1.596-2.623-.62-1.042-1.27-2.065-1.861-3.123-.49-.776-.871-1.61-1.26-2.439a31.568 31.568 0 0 1-.958-2.278 8.429 8.429 0 0 1-.606-2.12c-.123-.711-.261-1.419-.345-2.136-.03-.27-.04-.542-.051-.814L.573 66.97c.01.28.024.561.045.842.071.722.212 1.434.354 2.145.074.762.246 1.501.571 2.199.265.791.615 1.55.938 2.319.415.829.756 1.697 1.275 2.47.595 1.06 1.239 2.088 1.852 3.14.542.872 1.047 1.764 1.576 2.644.672.913 1.302 1.861 2.09 2.683.832.946 1.662 1.893 2.58 2.757.556.547 1.098 1.12 1.743 1.564 1.937 1.357 4.033 2.49 6.15 3.54 1.69.755 3.352 1.584 5.099 2.207 1.875.673 3.756 1.335 5.744 1.592 2.296.426 4.607.782 6.882 1.31.493.116 1.032.233 1.517.387.179.057.685.295.527.195-6.182-3.928-1.606-.587.821.415.198.077.414.035.625.026.173-.035.296-.086.39-.253a.757.757 0 0 1 .252-.286c.192-.262.205-.6.324-.9.25-.996.215-2.032.394-3.042.161-1.08.331-2.166.372-3.258.002-.356.007-.715.026-1.069.2.143.52.099.745.214.878.249 1.756.488 2.655.652 1.66.354 3.323.688 4.986 1.023 2.372.452 4.736.948 7.07 1.566 1.584.473 3.163.966 4.712 1.545.29.088.583.14.883.065.227-.066.392-.211.547-.385.274-.311.495-.672.664-1.05.25-.437.566-.835.77-1.297.222-.446.405-.92.577-1.386.293-.725.642-1.428.802-2.202.343-1.125.653-2.258.96-3.394a92.519 92.519 0 0 0 1.094-3.739c.14-.612.265-1.229.38-1.846.062-.22.16-.438.234-.642.05.125.475.009.6.046.658.006 1.317.03 1.972.095.798.091 1.583.258 2.382.345.875.128 1.736.336 2.615.436 1.458.24 2.904.563 4.328.959.961.285 1.912.61 2.88.868.263.01.524.009.786.015.113.001.225.012.337.004.163-.015.288-.066.386-.206.181-.225.338-.487.354-.78a17.07 17.07 0 0 1 .375-1.666c.1-.67.312-1.309.427-1.974.183-1.102.264-2.206.312-3.321.106-1.43.202-2.859.16-4.294.007-.637-.01-1.279.063-1.913.324.036.71.145 1.054.177 1.093.137 2.196.102 3.293.2 1.398.107 2.794.247 4.198.271 1.024.075 2.037.207 3.04.437.449.082.898.19 1.328.339.31.114.594.242.936.201.293.006.598.044.885-.028.35-.153.506-.484.509-.86.102-1.225.267-2.445.432-3.663.222-1.683.347-3.373.477-5.064.1-1.465.014-2.936.038-4.404-.003-1.735.004-3.47-.007-5.205-.019-1.88-.03-3.762-.136-5.64-.114-1.732-.275-3.46-.401-5.19-.138-1.27-.324-2.538-.614-3.781-.358-1.335-.654-2.687-1.034-4.017-.346-1.41-.729-2.81-1.054-4.224-.149-.582-.335-1.17-.436-1.755.003-.221.001-.442.003-.663.006-1.466-3.548-3.457-3.563-1.808-.095.7-.317 1.37-.574 2.027-.481 1.216-.97 2.428-1.571 3.592-.886 1.75-1.916 3.42-2.911 5.108a134.146 134.146 0 0 1-3.744 5.712c-.75 1.066-1.558 2.08-2.372 3.096-.078.109-.252.4-.425.402-.033.001.04-.159-.082-.055-.184-.019-.319.098-.419.246-.03.066-.079.127-.092.198-.03.15.493.731.491.89.613 1.074 1.206 2.158 1.856 3.21.896 1.39 1.804 2.772 2.704 4.16.455.787.91 1.575 1.345 2.372.072.093.118.225.226.27.062.026.327-.235-.058-.148-.472.003-.944.007-1.415.018a40.29 40.29 0 0 0-2.84.15c-1.491.147-2.98.296-4.48.308-1.938.11-3.87.299-5.808.41-1.681.117-3.36.278-5.03.51-.575.115-1.15.267-1.735.311-.255.042-.49.146-.62.383-.19.346-.234.658-.19 1.053.004.758.01 1.516.011 2.275.004 1.054 0 2.108.006 3.163.004 1.057.015 2.115.084 3.17.064.796.22 1.583.244 2.377-.005.183.034.496-.04.672-.013.028.016-.22-.074-.052-.38-.195-.809-.287-1.215-.413-1.336-.59-.29-.136 3.071 1.8.034.02-.06-.059-.098-.062-.034-.002-.057.055-.091.048-.06-.011.013-.178-.083-.162-3.669-2.212-2.34-1.722-4.4-2.343-1.469-.486-2.93-.996-4.413-1.44-2.458-.788-4.9-1.626-7.402-2.27-.872-.22-1.747-.434-2.614-.674-1.462-.405-3.43-1.594 2.25 1.543-1.259-.804-2.29-2.212-4.02-2.302-2.323-1.347 10.775 6.283-.527-.264-.38.213-.452.586-.594.972-.57 1.362-.939 2.793-1.426 4.186-.695 2.18-1.409 4.357-2.191 6.508-.578 1.501-1.221 2.98-1.679 4.525-.549 2.136-.085 1.055 3.242 2.884.026.015-.058-.007-.088-.01-.454-.17-.515-1.08-.777-1.45-.837-1.82-1.517-3.707-2.115-5.618-.524-1.62-1.192-3.234-1.34-4.944.007-.28.017-.561.014-.841-.001-.076-.022-.14 0-.21.048-.144.096-.113.211-.247.89-1.449-2.103-2.653-3.599-2.427a7.65 7.65 0 0 0-1.481.318c-.86.263-1.716.535-2.604.691a69.794 69.794 0 0 1-5.065.973c-2.51.297-5.038.461-7.567.324a138.055 138.055 0 0 1-10.994-.885c-2.933-.31-5.867-.71-8.82-.755l3.192 2.319z"
            ]
            []
        , Svg.path
            [ Gats.d "M3.202 68.925c.74-.01 1.48-.007 2.22.001.36 0 .719.006 1.078.005.272.003.544 0 .816 0 .258 0 .516 0 .775.002h.085L4.95 66.59h-.083c-.257.003-.514.002-.77.002-.272.002-.544-.002-.815 0-.355 0-.71.005-1.064.005-.74.009-1.479.011-2.218.001l3.202 2.326zm10.574-44.367c4.03-.1 8.06-.098 12.09-.106 4.428-.003 8.855.014 13.282.057.917.009 1.833.02 2.749.037.347.006.573-.002.87.167l-3.333-1.879c.122-.005.182-.069.19-.224.004-.073-.098.11-.148.164-.157.172-.327.327-.446.528-.252.399-.45.824-.59 1.274-.452 1.75-.813 3.521-1.236 5.279-.591 2.6-1.466 5.117-2.81 7.422a8.553 8.553 0 0 1-3.67 3.142c-1.444.541-2.204.495-3.645.011-2.235-.785-4.372-1.807-6.582-2.657a61.628 61.628 0 0 1-2.847-1.2c-.227-.103-.889-.449-.675-.321 3.565 2.126 3.512 2.118 2.279 1.206a6.355 6.355 0 0 1-1.7-2.322 25.94 25.94 0 0 1-.664-2.721c-.175-.953-.232-1.921-.261-2.888.032-.668-.066-1.32-.19-1.971a6.393 6.393 0 0 1-.132-.885 11.96 11.96 0 0 1-.01-.664v-.655l-3.546-1.807c0 .217 0 .435-.002.653 0 .228-.005.456.002.684.012.318.04.637.121.946.14.635.286 1.265.241 1.921.032.988.097 1.976.253 2.953.172.932.379 1.857.612 2.775.337.958.931 1.814 1.65 2.525 2.247 1.96 5.138 3.019 7.866 4.122 2.228.87 4.385 1.917 6.655 2.68 1.503.416 2.387.381 3.83-.238 1.532-.781 2.823-1.9 3.734-3.374 1.321-2.358 2.208-4.903 2.831-7.53.453-1.73.845-3.475 1.361-5.187.13-.419.326-.81.569-1.176.107-.171.277-.294.404-.45 1.365-1.36-2.44-2.198-3.25-2.572-1.167-.15-2.366.101-3.537.01-4.374.044-8.75.06-13.124.058-4.128-.008-8.256-.008-12.383-.106l3.192 2.319zm38.283-3.278c.198-.316.383-.64.595-.949.365-.53 1.747-2.358 2.075-2.796 1.523-2.032 3.024-4.08 4.544-6.114 2.124-2.838 4.445-5.52 6.754-8.21.323-.32.757-1.12 1.35-.96.35.094 1.117.607 1.347.748-.728-.36-1.376-1.178-2.182-1.08-.576.07.858.782 1.27 1.19a33.008 33.008 0 0 1 4.131 5.015c.94 1.536 1.744 3.17 1.98 4.97.043 1.086-.264 1.723-1.1 2.372-1.11.542-2.346.775-3.522 1.148-1.898.757-3.89 1.101-5.894 1.427-1.809.289-3.636.411-5.464.468-1.174.028-2.347.023-3.52.017l-2.72-.01c-.49-.002-.981-.004-1.472-.01l3.212 2.334c.482-.006.965-.007 1.448-.008h2.705c1.167 0 2.335-.001 3.502-.05 1.825-.087 3.647-.236 5.45-.535 2.025-.333 4.027-.725 5.955-1.45 1.229-.364 2.545-.567 3.653-1.24.86-.714 1.298-1.587 1.262-2.734-.188-1.88-1.037-3.579-2.002-5.182-.662-.985-.993-1.525-1.757-2.464-.592-.727-1.744-1.973-2.387-2.64-.395-.41-.741-.88-1.204-1.21-1.395-.997-2.897-1.835-4.345-2.751-.569-.292-.93-.586-1.58-.576-.668.011-1.074.798-1.435 1.224-2.16 2.798-4.394 5.54-6.514 8.368a1067.39 1067.39 0 0 1-7.518 9.696l3.383 1.992z"
            ]
            []
        , Svg.path
            [ Gats.fill "red"
            , Gats.d "M36.876 96.49c-.466-.18-3.268-.783-6.228-1.34-5.03-.948-8.404-1.962-12.065-3.626-4.26-1.936-12.052-12.88-13.714-19.261-.814-3.125-.79-3.18 1.274-2.977.978.096 4.978.493 8.89.883 9.133.91 16.473.93 20.862.055l3.251-.648.943 2.903c1.767 5.439 2.904 8.055 4.121 9.477 2.082 2.432 2.153 2.336 5.395-7.24 1.574-4.651 3.005-8.6 3.18-8.775.198-.198.483-.187.755.028.241.19 1.581.621 2.978.957 2.332.562 9.365 2.832 13.885 4.482 1.148.42 1.99.56 2.196.368.203-.19.289-2.74.22-6.534l-.111-6.223 1.81-.357c2.11-.416 8.578-.948 15.442-1.272 4.033-.19 4.826-.313 4.826-.751 0-.289-1.2-2.438-2.668-4.776-1.467-2.338-2.838-4.583-3.047-4.989-.334-.65-.008-1.29 2.76-5.418 1.728-2.575 3.823-5.86 4.656-7.301.834-1.441 1.603-2.622 1.708-2.625.331-.007 1.347 4.82 1.68 7.978.444 4.228.362 21.383-.12 24.868-.456 3.303.041 3.062-4.969 2.407-5.609-.734-10.313-.995-10.578-.588-.137.21-.25 1.982-.253 3.938-.005 3.913-.868 9.444-1.512 9.691-.225.087-1.846-.215-3.6-.67-3.914-1.015-10.02-1.995-11.518-1.849-1.08.106-1.126.175-1.729 2.649-1.407 5.776-3.173 10.678-4.613 12.81-.246.364-.842.283-3.146-.43-1.564-.483-6.043-1.575-9.955-2.426a1474.37 1474.37 0 0 1-7.858-1.724c-.878-.207-.911-.093-1.371 4.738-.174 1.824-.454 3.446-.623 3.605-.17.16-.686.143-1.154-.037zM26.547 40.008c-.931-.336-3.065-1.202-4.741-1.922-2.831-1.217-3.103-1.422-3.821-2.868-.58-1.167-.866-2.608-1.144-5.757-.205-2.32-.541-4.311-.75-4.449-.21-.137 4.697-.251 10.902-.254 10.588-.005 11.273.032 11.11.588-.096.326-.56 2.175-1.03 4.11-1.444 5.946-3.308 9.173-6.069 10.51-1.733.839-2.24.844-4.457.042zm28.181-22.113c.178-.233 1.936-2.557 3.906-5.164 4.571-6.05 8.014-10.056 8.505-9.896.723.236 4.345 4.812 5.302 6.7 2.256 4.449 1.703 5.496-3.731 7.057-4.134 1.187-7.75 1.727-11.58 1.727-2.129 0-2.656-.093-2.402-.424z"
            ]
            []
        ]
    
