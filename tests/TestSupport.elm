module TestSupport exposing (..)

import Test.Html.Query exposing (fromHtml)
import Test.Html.Event exposing (toResult)
import Expect exposing (Expectation)
import Html exposing (Html)


type alias TestData model msg =
    { expectations : List Expectation
    , view : model -> Html msg
    , model : model
    , update : msg -> model -> ( model, Cmd msg )
    }


seed view update model =
    { expectations = [ Expect.pass ]
    , view = view
    , update = update
    , model = model
    }


execute library expectation testData =
    let
        view =
            testData.view testData.model

        updatedModel =
            (fromHtml view)
                |> library
                |> toResult
                |> Result.map (testUpdate testData.update testData.model)
    in
        Result.map
            (\model ->
                model
                    |> testData.view
                    |> fromHtml
                    |> expectation
                    |> (\newExpectation -> { testData | expectations = newExpectation :: testData.expectations, model = model })
            )
            updatedModel


runTests result =
    case result of
        Ok testData ->
            List.foldr andAlso Expect.pass testData.expectations

        Err error ->
            Expect.fail error


andAlso : Expectation -> Expectation -> Expectation
andAlso l r =
    Expect.all [ always l, always r ] ()


testUpdate update model msg =
    Tuple.first (update msg model)


testView view model =
    fromHtml (view model)


andThenExpect : (a -> Expectation) -> (Result String a -> Expectation)
andThenExpect expectation model =
    let
        result =
            Result.map expectation model
    in
        case result of
            Ok expectation ->
                expectation

            Err error ->
                Expect.fail error


expect : (a -> Expectation) -> (Result String a -> Expectation)
expect expectation model =
    let
        result =
            Result.map expectation model
    in
        case result of
            Ok expectation ->
                expectation

            Err error ->
                Expect.fail error


uiTest view update library model =
    model
        |> testView view
        |> (library >> toResult)
        |> Result.map (testUpdate update model)
