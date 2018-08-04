module AppTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Test.Html.Query as Query exposing (..)
import Test.Html.Event as Event exposing (..)
import Test.Html.Selector exposing (..)
import TestSupport
    exposing
        ( andThenExpect
        , testProgram
        , runTests
        , executeAction
        , expectView
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


subject =
    testProgram App.view App.update


suite : Test
suite =
    describe "Application"
        [ test "increments the current count by 1" <|
            \_ ->
                subject model
                    |> expectView
                        (counter >> has [ text "0" ])
                    |> andThenExecuteAction
                        clickIncrement
                    |> andThenExecuteAction
                        clickIncrement
                    |> andThenExpect
                        (counter >> has [ text "2" ])
                    |> andThenExecuteAction
                        clickIncrement
                    |> andThenExecuteAction
                        clickIncrement
                    |> andThenExpect
                        (counter >> has [ text "4" ])
                    |> andThenExecuteAction
                        clickDecrement
                    |> andThenExpect
                        (counter >> has [ text "3" ])
                    |> runTests
        ]
