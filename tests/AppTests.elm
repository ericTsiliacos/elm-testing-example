module AppTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Test.Html.Query as Query exposing (..)
import Test.Html.Event as Event exposing (..)
import Test.Html.Selector exposing (..)
import TestContext exposing (clickButton, expectView, create, expectLastEffect)
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
    describe "Application"
        [ test "increments the current count by 1" <|
            \_ ->
                create
                    { init = App.init
                    , update = App.update
                    , view = App.view
                    }
                    |> simulate click (App.view |> fromHtml |> incrementButton)
                    |> expectLastEffect (Expect.equal (HttpGet getWarAndPeace NewBook))
          -- |> expectView (counter >> has [ text "1" ])
        ]
