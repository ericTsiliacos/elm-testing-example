module App exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Http exposing (..)


httpGet request msg =
    Http.send msg request


init =
    ( { count = 0 }, None )


type alias Model =
    { count : Int }


toCmd : Effect Msg -> Cmd Msg
toCmd effect =
    case effect of
        HttpGet request msg ->
            httpGet request msg

        None ->
            Cmd.none


type Effect msg
    = HttpGet (Http.Request String) (Result Error String -> msg)
    | None


type Msg
    = Increment
    | Decrement
    | NewBook (Result Http.Error String)


getWarAndPeace : Http.Request String
getWarAndPeace =
    Http.getString "https://example.com/books/war-and-peace"


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }, HttpGet getWarAndPeace NewBook )

        Decrement ->
            if model.count == 0 then
                ( model, None )
            else
                ( { model | count = model.count - 1 }, None )

        NewBook result ->
            ( model, None )


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text ("count: " ++ (toString model.count)) ]
        , div [] [ text ("multiplier: " ++ (toString (model.count * 2))) ]
        , button [ onClick Increment ] [ text "+" ]
        , button [ onClick Decrement ] [ text "-" ]
        ]
