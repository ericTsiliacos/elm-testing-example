module AppTests exposing (application, bookTitle, counter, decrement, increment, multiplier, suite)

import App exposing (..)
import Expect exposing (Expectation)
import Result exposing (andThen, map)
import Test exposing (..)
import Test.Html.Query as Query exposing (..)
import Test.Html.Selector exposing (..)
import TestSupport
    exposing
        ( andExpectLastEffect
        , click
        , executeTests
        , expectView
        , testProgram
        , updateWith
        )


increment =
    find
        [ tag "button"
        , containing [ text "+" ]
        ]


decrement =
    find
        [ tag "button"
        , containing [ text "-" ]
        ]


bookTitle =
    find
        [ tag "div"
        , containing [ text "book title: " ]
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


application =
    testProgram App.view App.update App.init


suite : Test
suite =
    describe "Application"
        [ test "increments the current count by 1" <|
            \_ ->
                application
                    |> click increment
                    |> andExpectLastEffect (Expect.equal (HttpGet getWarAndPeace NewBook))
                    |> andThen (updateWith (NewBook (Ok "hello world")))
                    |> andThen (expectView (bookTitle >> has [ text "hello world" ]))
                    |> andThen (click increment)
                    |> andThen
                        (expectView
                            (Expect.all
                                [ counter >> has [ text "2" ]
                                , multiplier >> has [ text "4" ]
                                ]
                            )
                        )
                    |> andThen (click decrement)
                    |> andThen (expectView (counter >> has [ text "1" ]))
                    |> executeTests
        ]
