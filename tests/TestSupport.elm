module TestSupport exposing (..)

import Test.Html.Query exposing (fromHtml)
import Test.Html.Event exposing (toResult)


testUpdate update model =
    \msg ->
        toResult msg
            |> (\result ->
                    case result of
                        Ok msg ->
                            update msg model

                        Err error ->
                            Debug.crash error
               )
            |> Tuple.first


testView view =
    \model -> view model |> fromHtml


uiTest view update library =
    \model ->
        (testView view) model
            |> library
            |> (testUpdate update) model
