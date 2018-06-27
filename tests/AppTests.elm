module AppTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Test.Html.Query as Query exposing (..)
import Test.Html.Event as Event exposing (..)
import Test.Html.Selector exposing (..)
import TestSupport exposing (..)
import App exposing (..)


incrementButton =
    find
        [ tag "button"
        , containing [ text "increment" ]
        ]


decrementButton =
    find
        [ tag "button"
        , containing [ text "decrement" ]
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


suite : Test
suite =
    describe "Application"
        [ describe "initial state"
            [ test "current count is 0" <|
                \_ ->
                    model
                        |> view
                        |> counter
                        |> has [ text "0" ]
            ]
        , describe "clicking on the increment button"
            [ test "increments the current count by 1" <|
                \_ ->
                    model
                        |> run clickIncrement
                        |> run clickIncrement
                        |> view
                        |> counter
                        |> has [ text "2" ]
            ]
        , describe "clicking on the decrement button"
            [ test "decrements the current count by 1" <|
                \_ ->
                    { model | count = 3 }
                        |> run clickDecrement
                        |> run clickDecrement
                        |> view
                        |> Expect.all
                            [ counter >> has [ text "1" ]
                            , multiplier >> has [ text "2" ]
                            ]
            , test "counter does not go below 0" <|
                \_ ->
                    { model | count = 1 }
                        |> run clickDecrement
                        |> run clickDecrement
                        |> view
                        |> counter
                        |> has [ text "0" ]
            ]
        ]
