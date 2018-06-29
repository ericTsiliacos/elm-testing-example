module AppTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Test.Html.Query as Query exposing (..)
import Test.Html.Event as Event exposing (..)
import Test.Html.Selector exposing (..)
import TestSupport exposing (uiTest, testView, expect)
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


suite : Test
suite =
    let
        view =
            testView App.view

        expectView expectation =
            TestSupport.expect (view >> expectation)

        run =
            uiTest App.view App.update
    in
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
                            |> run clickIncrement
                            |> andThen (run clickIncrement)
                            |> andThen (run clickIncrement)
                            |> expectView
                                (counter
                                    >> has [ text "3" ]
                                )
                ]
            , describe "clicking on the decrement button"
                [ test "decrements the current count by 1" <|
                    \_ ->
                        { model | count = 3 }
                            |> run clickDecrement
                            |> andThen (run clickDecrement)
                            |> expectView
                                (Expect.all
                                    [ counter >> has [ text "1" ]
                                    , multiplier >> has [ text "2" ]
                                    ]
                                )
                , test "counter does not go below 0" <|
                    \_ ->
                        { model | count = 1 }
                            |> run clickDecrement
                            |> andThen (run clickDecrement)
                            |> expectView
                                (counter
                                    >> has [ text "0" ]
                                )
                ]
            ]
