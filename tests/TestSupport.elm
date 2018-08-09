module TestSupport exposing (..)

import Test.Html.Query exposing (fromHtml)
import Test.Html.Event as Event exposing (toResult, click, simulate)
import Expect exposing (Expectation)
import Html exposing (Html)


type alias TestData model msg effect =
    { expectations : List Expectation
    , view : model -> Html msg
    , model : model
    , update : msg -> model -> ( model, effect )
    , effect : effect
    }


type Program model msg
    = Program { model : model, view : Test.Html.Query.Single msg }


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


andExpectLastEffect expectation testDataResult =
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


verify expectation testData =
    Program { model = testData.model, view = testData.model |> testData.view |> fromHtml }
        |> expectation
        |> (\newExpectation -> { testData | expectations = newExpectation :: testData.expectations })
        |> (\testData -> Ok testData)


updateWith msg testData =
    testData.update msg testData.model
        |> (\( model, effect ) -> Ok { testData | model = model, effect = effect })


expectView expectation testData =
    testView expectation testData


thenView expectation testDataResult =
    Result.andThen (testView expectation) testDataResult


click element =
    element >> simulate Event.click |> run


executeTests result =
    case result of
        Ok testData ->
            List.foldr andAlso Expect.pass testData.expectations

        Err error ->
            Expect.fail error


andAlso : Expectation -> Expectation -> Expectation
andAlso l r =
    Expect.all [ always l, always r ] ()
