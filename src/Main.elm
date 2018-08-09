module Main exposing (..)

import App
import Html


main =
    Html.program
        { init = App.init |> Tuple.mapSecond App.toCmd
        , update =
            (\msg model ->
                App.update msg model
                    |> (\( model, effect ) -> ( model, App.toCmd effect ))
            )
        , subscriptions = always Sub.none
        , view = App.view
        }
