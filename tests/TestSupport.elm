module TestSupport exposing (andExpectLastEffect, click, executeTests, expectView, testProgram, updateWith)

import Expect exposing (Expectation)
import Html exposing (Html)
import Test.Html.Event as Event exposing (click, simulate, toResult)
import Test.Html.Query exposing (fromHtml)


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
            { testData | expectations = expectation testData.effect :: testData.expectations }
        )
        testDataResult


testView expectation testData =
    testData.model
        |> testData.view
        |> fromHtml
        |> expectation
        |> (\newExpectation -> { testData | expectations = newExpectation :: testData.expectations })
        |> Ok


updateWith msg testData =
    testData.update msg testData.model
        |> (\( model, effect ) -> Ok { testData | model = model, effect = effect })


expectView expectation testData =
    testView expectation testData


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
