module Game exposing (Model, init, view, update, subscriptions)

import Aviary.Birds exposing (cardinal, dove, eagle)
import Array exposing (Array)
import Constants as C
import Frame2d
import Graphics
import Html exposing (Html)
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import Logic
import Msg
import Polygon2d
import Point2d
import Svg
import Svg.Attributes as Gats
import Dict exposing (Dict)
import Map.Room exposing (Room)
import Map.Level exposing (Level)
import Html.Events


type alias Controls = List Key


type Model
    = Loading
        { width : Int
        , height : Int
        }
    | MainMenu
        { levels : List Level
        , width : Int
        , height : Int
        }
    | Game 
        { logic : Logic.Model 
        , gfx : Graphics.Model 
        , controls : Controls
        , levels : List Level
        }



view : Model -> Html Msg.Msg
view model =
    case model of 
        Loading _ ->
            Html.main_ [] [ Html.text "Loading" ]
        MainMenu {levels} ->
            levels
            |> List.map (\l -> 
                Html.li []
                    [ Html.button
                        [ Html.Events.onClick (Msg.SelectLevel l) ]
                        [ Html.text l.name ]
                    ]
                )
            |> Html.ul []
            |> List.singleton
            |> Html.main_ []
        Game {gfx} ->
            Graphics.view gfx


update : Msg.Msg -> Model -> (Model, Cmd Msg.Msg)
update msg model =
    case model of
        Loading {width, height} ->
            case msg of 
                Msg.LevelLoaded level ->
                    ( MainMenu { levels = [level], width = width, height = height }
                    , Cmd.none
                    )
                Msg.LevelLoadingError err ->
                    let
                        m = Debug.log "Error" err
                    in
                    ( model, Cmd.none )
                _ -> ( model, Cmd.none )

        MainMenu {levels, width, height} ->
            case msg of 
                Msg.SelectLevel level -> 
                    Logic.init level
                    |> Maybe.map(\l ->
                        let
                            graphics = Graphics.init width height l
                        in
                        Game { logic = l, gfx = graphics, controls = [], levels = levels }
                        )
                    |> Maybe.withDefault model
                    |> cardinal Tuple.pair Cmd.none

                _ -> (model, Cmd.none)

        Game ({logic, gfx, controls, levels} as data) ->
            case msg of
                Msg.KeyMsg km ->
                    let 
                        (newKeys, wasChanged) = Keyboard.updateWithKeyChange (Keyboard.oneOf 
                            [Keyboard.Arrows.arrowKey, Keyboard.whitespaceKey]) km controls
                    in
                    case wasChanged of
                        Just (Keyboard.KeyDown Keyboard.Spacebar) ->
                            let
                                newLogic = Logic.update Logic.Jump logic
                                newGfx = Graphics.update Graphics.Leap logic newKeys gfx
                            in
                            Tuple.mapFirst (\l -> Game {data | logic = l, gfx = newGfx, controls = newKeys }) newLogic
                        _ ->
                            Game 
                                { data 
                                | gfx = (Graphics.update (Graphics.Lean (Keyboard.Arrows.arrowsDirection newKeys)) logic newKeys gfx)
                                , controls = newKeys 
                                }
                            |> cardinal Tuple.pair Cmd.none
                Msg.Frame newTime ->
                    Game { data | gfx = (Graphics.update (Graphics.Frame newTime) logic controls gfx) }
                    |> cardinal Tuple.pair Cmd.none
                Msg.Resize w h ->
                    Game { data | gfx = (Graphics.update (Graphics.Resize w h) data.logic data.controls data.gfx) }
                    |> cardinal Tuple.pair Cmd.none
                Msg.LevelLoadingError err ->
                    (model, Cmd.none)
                Msg.LevelLoaded level ->
                    ( Game { data | logic = (Logic.setMap level logic) }
                    , Cmd.none
                    )
                _ ->
                    ( model, Cmd.none )
             

        --_ -> (Game logic gfx controls, Cmd.none)

subscriptions model = 
    case model of
        Game {logic, gfx} ->
            Sub.batch
                [ Logic.subscriptions logic
                , Graphics.subscriptions gfx
               -- , Msg.animationFrameSubscription
                , Sub.map Msg.KeyMsg Keyboard.subscriptions
                , Msg.levelListener
                ]
        _ ->
            Sub.batch [ Msg.levelListener ]
            
init : Int -> Int -> Model
init w h = 
    Loading { width = w, height = h }  

