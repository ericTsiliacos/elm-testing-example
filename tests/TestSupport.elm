module TestSupport exposing
    ( andExpectLastEffect
    , click
    , executeTests
    , expectView
    , testProgram
    , updateWith
    )

import Expect exposing (Expectation)
import Html exposing (Html)
import Test.Html.Event as Event exposing (Event, click, simulate, toResult)
import Test.Html.Query exposing (Single, fromHtml)


type alias TestData model msg effect =
    { expectations : List Expectation
    , view : model -> Html msg
    , model : model
    , update : msg -> model -> ( model, effect )
    , effect : effect
    }


testProgram :
    { view : model -> Html msg
    , update : msg -> model -> ( model, effect )
    , init : ( model, effect )
    }
    -> TestData model msg effect
testProgram { view, update, init } =
    let
        ( model, effect ) =
            init
    in
    { expectations = [ Expect.pass ]
    , view = view
    , update = update
    , model = model
    , effect = effect
    }


run :
    (Single msg -> Event msg)
    -> TestData model msg effect
    -> Result String (TestData model msg effect)
run action testData =
    testData.model
        |> testData.view
        |> fromHtml
        |> action
        |> toResult
        |> Result.map (\msg -> testData.update msg testData.model)
        |> Result.map (\( updatedModel, effect ) -> { testData | model = updatedModel, effect = effect })


andExpectLastEffect :
    (effect -> Expectation)
    -> Result String (TestData model msg effect)
    -> Result String (TestData model msg effect)
andExpectLastEffect expectation testDataResult =
    Result.map
        (\testData ->
            { testData | expectations = expectation testData.effect :: testData.expectations }
        )
        testDataResult


testView :
    (Single msg -> Expectation)
    -> TestData model msg effect
    -> Result String (TestData model msg effect)
testView expectation testData =
    testData.model
        |> testData.view
        |> fromHtml
        |> expectation
        |> (\newExpectation -> { testData | expectations = newExpectation :: testData.expectations })
        |> Ok


updateWith :
    msg
    -> TestData model msg effect
    -> Result String (TestData model msg effect)
updateWith msg testData =
    testData.update msg testData.model
        |> (\( model, effect ) -> Ok { testData | model = model, effect = effect })


expectView :
    (Single msg -> Expectation)
    -> TestData model msg effect
    -> Result String (TestData model msg effect)
expectView expectation testData =
    testView expectation testData


click :
    (Single msg -> Single msg)
    -> TestData model msg effect
    -> Result String (TestData model msg effect)
click element =
    element >> simulate Event.click |> run


executeTests : Result String (TestData model msg effect) -> Expectation
executeTests result =
    case result of
        Ok testData ->
            List.foldr andAlso Expect.pass testData.expectations

        Err error ->
            Expect.fail error


andAlso : Expectation -> Expectation -> Expectation
andAlso l r =
    Expect.all [ always l, always r ] ()
