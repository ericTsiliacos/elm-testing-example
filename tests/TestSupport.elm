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


testProgram view update model =
    { expectations = [ Expect.pass ]
    , view = view
    , update = update
    , model = model
    }


run action testData =
    testData.model
        |> testData.view
        |> fromHtml
        |> action
        |> toResult
        |> Result.map (testUpdate testData.update testData.model)
        |> Result.map (\updatedModel -> { testData | model = updatedModel })


andThenRun action testDataResult =
    Result.andThen (run action) testDataResult


testView expectation testData =
    testData.model
        |> testData.view
        |> fromHtml
        |> expectation
        |> (\newExpectation -> { testData | expectations = newExpectation :: testData.expectations })
        |> (\testData -> Ok testData)


andThenView expectation testDataResult =
    Result.andThen (testView expectation) testDataResult


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
