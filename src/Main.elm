module Main exposing (..)

import App
import Html


main =
    Html.program
        { init = ( App.model, Cmd.none )
        , update = App.update
        , subscriptions = always Sub.none
        , view = App.view
        }
