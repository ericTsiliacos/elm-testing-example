module TestSupport exposing (..)

import Test.Html.Query exposing (fromHtml)
import Test.Html.Event exposing (toResult)
import Expect


testUpdate update model msg =
    Tuple.first (update msg model)


testView view model =
    fromHtml (view model)


expect expectation =
    \model ->
        let
            result =
                Result.map expectation model
        in
            case result of
                Ok expectation ->
                    expectation

                Err error ->
                    Expect.fail error


uiTest view update library model =
    model
        |> testView view
        |> (library >> toResult)
        |> Result.map (testUpdate update model)
