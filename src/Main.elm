module Main exposing (..)

import App
import Html


main =
    Html.program
        { init = App.init >> Tuple.mapSecond toCmd
        , update = App.update >> Tuple.mapSecond toCmd
        , subscriptions = always Sub.none
        , view = App.view
        }
