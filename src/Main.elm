module Main exposing (main)

import App
import Browser
import Html


main : Program {} App.Model App.Msg
main =
    Browser.element
        { init = \_ -> App.init |> Tuple.mapSecond App.toCmd
        , view = App.view
        , update =
            \msg model ->
                App.update msg model
                    |> (\( updatedModel, effect ) -> ( updatedModel, App.toCmd effect ))
        , subscriptions = always Sub.none
        }
