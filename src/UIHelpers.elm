module UIHelpers exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode


createMenu : (innerMsg -> msg) -> innerMsg -> String -> Bool -> Html msg
createMenu fMsg iMsg title isSelected =
    if isSelected then
        a [ href "#" ] [ text title ]

    else
        a [ href "#", onClick (fMsg iMsg) ] [ text title ]


selectOfItems : List a -> (String -> msg) -> (a -> ( String, String )) -> Maybe a -> Html msg
selectOfItems items fMsg fKeyText selected =
    select
        (List.filterMap identity
            [ Just (onInput fMsg)
            , selected |> Maybe.map (fKeyText >> Tuple.first >> value)
            ]
        )
        (items
            |> List.map
                (\item ->
                    let
                        ( k, t ) =
                            fKeyText item
                    in
                    option [ value k ] [ text t ]
                )
        )
