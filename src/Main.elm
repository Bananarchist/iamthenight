module Main exposing (main)

import App
import Browser
import Model exposing (Model)
import Msg exposing (Msg)


main : Program { width : Int, height : Int } App.Model Msg
main =
    Browser.element
        { init = App.init
        , subscriptions = App.subscriptions
        , update = App.update
        , view = App.view
        }
