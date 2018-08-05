module TestSupport exposing (..)

import Test.Html.Query exposing (fromHtml)
import Test.Html.Event exposing (toResult)
import Expect exposing (Expectation)
import Html exposing (Html)


type alias TestData model msg effect =
    { expectations : List Expectation
    , view : model -> Html msg
    , model : model
    , update : msg -> model -> ( model, effect )
    , effect : effect
    }


testProgram view update ( model, effect ) =
    { expectations = [ Expect.pass ]
    , view = view
    , update = update
    , model = model
    , effect = effect
    }


run action testData =
    testData.model
        |> testData.view
        |> fromHtml
        |> action
        |> toResult
        |> Result.map (\msg -> testData.update msg testData.model)
        |> Result.map (\( updatedModel, effect ) -> { testData | model = updatedModel, effect = effect })


andExpectEffect expectation testDataResult =
    Result.map
        (\testData ->
            { testData | expectations = (expectation testData.effect) :: testData.expectations }
        )
        testDataResult


thenRun action testDataResult =
    Result.andThen (run action) testDataResult


testView expectation testData =
    testData.model
        |> testData.view
        |> fromHtml
        |> expectation
        |> (\newExpectation -> { testData | expectations = newExpectation :: testData.expectations })
        |> (\testData -> Ok testData)


thenView expectation testDataResult =
    Result.andThen (testView expectation) testDataResult


executeTests result =
    case result of
        Ok testData ->
            List.foldr andAlso Expect.pass testData.expectations

        Err error ->
            Expect.fail error


andAlso : Expectation -> Expectation -> Expectation
andAlso l r =
    Expect.all [ always l, always r ] ()
