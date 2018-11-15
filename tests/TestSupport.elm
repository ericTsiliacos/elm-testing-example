module TestSupport exposing
    ( andExpectLastEffect
    , click
    , executeTests
    , expectView
    , initTestProgram
    , updateWith
    )

{-| This library provides a way for testing the elm architecture while also
making it possible to make more than one assertion in a test.
-}

import Expect exposing (Expectation)
import Html exposing (Html)
import Test.Html.Event as Event exposing (Event, click, simulate, toResult)
import Test.Html.Query exposing (Single, fromHtml)


type TestData model msg effect
    = TestData
        { expectations : List Expectation
        , view : model -> Html msg
        , model : model
        , update : msg -> model -> ( model, effect )
        , effect : effect
        }


{-| Creates a testable application from your view, update, and init functions.

    application =
        initTestProgram
            { view = App.view
            , update = App.update
            , init = App.init
            }

-}
initTestProgram :
    { view : model -> Html msg
    , update : msg -> model -> ( model, effect )
    , init : ( model, effect )
    }
    -> TestData model msg effect
initTestProgram { view, update, init } =
    let
        ( model, effect ) =
            init
    in
    TestData
        { expectations = [ Expect.pass ]
        , view = view
        , update = update
        , model = model
        , effect = effect
        }


{-| Test what the last effect should be. Note, this library assumes that you
have abstracted Cmd msg to some bounder of the application, and are only dealing
with effects.

application
|> click increment
|> andExpectLastEffect (Expect.equal (HttpGet getWarAndPeace NewBook))
|> executeTests

-}
andExpectLastEffect :
    (effect -> Expectation)
    -> Result String (TestData model msg effect)
    -> Result String (TestData model msg effect)
andExpectLastEffect expectation testDataResult =
    Result.map
        (\testData ->
            let
                (TestData internalTestData) =
                    testData
            in
            TestData { internalTestData | expectations = expectation internalTestData.effect :: internalTestData.expectations }
        )
        testDataResult


testView :
    (Single msg -> Expectation)
    -> TestData model msg effect
    -> Result String (TestData model msg effect)
testView expectation testData =
    let
        (TestData internalTestData) =
            testData
    in
    internalTestData.model
        |> internalTestData.view
        |> fromHtml
        |> expectation
        |> (\newExpectation -> TestData { internalTestData | expectations = newExpectation :: internalTestData.expectations })
        |> Ok


updateWith :
    msg
    -> TestData model msg effect
    -> Result String (TestData model msg effect)
updateWith msg testData =
    let
        (TestData internalTestData) =
            testData
    in
    internalTestData.update msg internalTestData.model
        |> (\( model, effect ) ->
                Ok
                    (TestData
                        { internalTestData
                            | model = model
                            , effect = effect
                        }
                    )
           )


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
        Ok (TestData internalTestData) ->
            List.foldr andAlso Expect.pass internalTestData.expectations

        Err error ->
            Expect.fail error


run :
    (Single msg -> Event msg)
    -> TestData model msg effect
    -> Result String (TestData model msg effect)
run action testData =
    let
        (TestData internalTestData) =
            testData
    in
    internalTestData.model
        |> internalTestData.view
        |> fromHtml
        |> action
        |> toResult
        |> Result.map (\msg -> internalTestData.update msg internalTestData.model)
        |> Result.map (\( updatedModel, effect ) -> TestData { internalTestData | model = updatedModel, effect = effect })


andAlso : Expectation -> Expectation -> Expectation
andAlso l r =
    Expect.all [ always l, always r ] ()
