module Shared exposing (..)

import Dict
import Http
import List


type alias HttpResult value =
    Result Http.Error value


type alias HttpState value =
    Maybe (HttpResult value)


errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "The URL " ++ url ++ " was invalid"

        Http.Timeout ->
            "Unable to reach the server, try again"

        Http.NetworkError ->
            "Unable to reach the server, check your network connection"

        Http.BadStatus 500 ->
            "The server had a problem, try again later"

        Http.BadStatus 400 ->
            "Verify your information and try again"

        Http.BadStatus _ ->
            "Unknown error"

        Http.BadBody errorMessage ->
            errorMessage


splitAt : Int -> List a -> List (List a)
splitAt i list =
    case List.take i list of
        [] ->
            []

        listHead ->
            listHead :: splitAt i (List.drop i list)


formatInt : Int -> String
formatInt x =
    x
        |> String.fromInt
        |> String.toList
        |> List.map String.fromChar
        |> List.reverse
        |> splitAt 3
        |> List.map (List.reverse >> String.join "")
        |> List.reverse
        |> String.join ","


formatMoney : Int -> String
formatMoney x =
    "$" ++ formatInt x


maybeValueString : String -> Maybe String
maybeValueString x =
    -- would love to add whitespace check at some point
    if String.isEmpty x then
        Nothing

    else
        Just x



-- addSertDict : key -> value -> Dict.Dict key value -> Dict.Dict key value


foldSertDict : (a -> a -> a) -> comparable -> a -> Dict.Dict comparable a -> Dict.Dict comparable a
foldSertDict f k v dict =
    case Dict.get k dict of
        Nothing ->
            Dict.insert k v dict

        Just v1 ->
            Dict.insert k (f v1 v) dict
