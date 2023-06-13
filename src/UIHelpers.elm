module UIHelpers exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)


createMenu : (innerMsg -> msg) -> innerMsg -> String -> Bool -> Html msg
createMenu fMsg iMsg title isSelected =
    if isSelected then
        a [ href "#" ] [ text title ]

    else
        a [ href "#", onClick (fMsg iMsg) ] [ text title ]
