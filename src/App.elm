module App exposing (init, subscriptions, update, view, Model)

import Aviary.Birds exposing (cardinal, kestrel, eagle)
import Html exposing (Html)
import Msg exposing (Msg)
import Game
import Browser.Events


type Model
    = App Game.Model

init : { width : Int, height : Int } -> ( Model, Cmd Msg )
init {width, height} =
    ( App (Game.init width height), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ((App gm) as model) =
    case msg of
        _ ->
            Game.update msg gm
            |> Tuple.mapFirst (App)


subscriptions : Model -> Sub Msg
subscriptions (App gm) =
    Sub.batch
        [ Game.subscriptions gm
        ]


view : Model -> Html Msg
view (App gm) =
    Game.view gm
