module App exposing (..)

import Html exposing (..)
import Html.Events exposing (..)


model =
    { count = 0 }


type alias Model =
    { count : Int }


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            if model.count == 0 then
                ( model, Cmd.none )
            else
                ( { model | count = model.count - 1 }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text ("count: " ++ (toString model.count)) ]
        , div [] [ text ("multiplier: " ++ (toString (model.count * 2))) ]
        , button [ onClick Increment ] [ text "+" ]
        , button [ onClick Decrement ] [ text "-" ]
        ]
