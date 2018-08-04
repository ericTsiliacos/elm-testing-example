module AppTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Test.Html.Query as Query exposing (..)
import Test.Html.Event as Event exposing (..)
import Test.Html.Selector exposing (..)
import TestSupport
    exposing
        ( uiTest
        , testView
        , expect
        , andAlso
        , andThenExpectView
        , seed
        , TestData
        , execute
        , runTests
        , executeAction
        , andThenExecuteAction
        )
import Result exposing (andThen, map)
import App exposing (..)


incrementButton =
    find
        [ tag "button"
        , containing [ text "+" ]
        ]


decrementButton =
    find
        [ tag "button"
        , containing [ text "-" ]
        ]


counter =
    find
        [ tag "div"
        , containing [ text "count" ]
        ]


multiplier =
    find
        [ tag "div"
        , containing [ text "multiplier" ]
        ]


clickIncrement =
    incrementButton >> simulate click


clickDecrement =
    decrementButton >> simulate click


view =
    testView App.view


run =
    uiTest App.view App.update


subject =
    seed App.view App.update


suite : Test
suite =
    describe "Application"
        [ describe "initial state"
            [ test "current count is 0" <|
                \_ ->
                    model
                        |> (view >> counter)
                        |> has [ text "0" ]
            ]
        , describe "clicking on the increment button"
            [ test "increments the current count by 1" <|
                \_ ->
                    model
                        |> subject
                        |> executeAction clickIncrement
                        |> andThenExecuteAction clickIncrement
                        |> andThenExpectView (counter >> has [ text "2" ])
                        |> andThenExecuteAction clickIncrement
                        |> andThenExpectView (counter >> has [ text "3" ])
                        |> andThenExecuteAction clickDecrement
                        |> runTests
            ]
          -- , describe "clicking on the decrement button"
          --     [ test "decrements the current count by 1" <|
          --         \_ ->
          --             { model | count = 3 }
          --                 |> run clickDecrement
          --                 |> andThen (run clickDecrement)
          --                 |> TestSupport.expect
          --                     (view
          --                         >> Expect.all
          --                             [ counter >> has [ text "1" ]
          --                             , multiplier >> has [ text "2" ]
          --                             ]
          --                     )
          --     , test "counter does not go below 0" <|
          --         \_ ->
          --             { model | count = 1 }
          --                 |> run clickDecrement
          --                 |> andThen (run clickDecrement)
          --                 |> TestSupport.expect
          --                     (view
          --                         >> counter
          --                         >> has [ text "0" ]
          --                     )
          --     , test "foo" <|
          --         \_ ->
          --             1 |> Expect.equal 1 |> andAlso (2 |> Expect.equal 2)
          --     ]
        ]
