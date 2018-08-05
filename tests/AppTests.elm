module AppTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Test.Html.Query as Query exposing (..)
import Test.Html.Event as Event exposing (..)
import Test.Html.Selector exposing (..)
import TestSupport
    exposing
        ( thenView
        , testProgram
        , executeTests
        , run
        , testView
        , thenRun
        , andExpectEffect
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
                subject ( model, None )
                    |> run clickIncrement
                    |> andExpectEffect (Expect.equal (HttpGet getWarAndPeace NewBook))
                    |> thenView (counter >> has [ text "1" ])
                    |> (thenRun clickDecrement
                            >> thenRun clickIncrement
                            >> thenRun clickIncrement
                       )
                    |> thenView (counter >> has [ text "2" ])
                    |> (thenRun clickIncrement >> thenRun clickIncrement)
                    |> thenView (counter >> has [ text "4" ])
                    |> thenRun clickDecrement
                    |> thenView (counter >> has [ text "3" ])
                    |> executeTests
        ]
