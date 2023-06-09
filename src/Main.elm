module Main exposing (..)

import Dict

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D
import Json.Decode exposing (Decoder)

-- chaining https://allo-media.net/en/tech/learning/elm/2018/02/05/chaining-http-requests-in-elm.html

type BookState
  = NotStarted
  | Started
  | Finished (Result Http.Error String)

type alias Model =
  { counter : Int
    , bookState: BookState
    -- , gettingBook : Bool
    -- , gotBookResult : Maybe (Result Http.Error String)
    , gotItems: Maybe (Result Http.Error (List String))
  }

type alias GoodInfo = {
  cost: Int
  , soldBy: String
  }

type Msg
  = GetBook
  | GotBook (Result Http.Error String)
  | GotItems (Result Http.Error (List String))
  | GotEconomy (Result Http.Error (Dict.Dict String GoodInfo))
  | Increment
  | Decrement
  | Reset
initialState = { counter = 0, bookState = NotStarted, gotItems = Nothing }
initialCmd = Http.get {
  url = "json/economy.json"
  , expect = Http.expectJson GotEconomy economyDecoder
    -- , expect = Http.expectJson GotItems (D.list (D.field "name" D.string))
  }
main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }

init : () -> (Model, Cmd Msg)
init _ = ( initialState, initialCmd )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetBook ->
      Debug.log "Getting book!" -- https://riptutorial.com/elm/topic/2845/debugging
      ({model | bookState = Started}, getBook)
    Increment ->
      ({ model | counter = model.counter + 1 }, Cmd.none)
    Decrement ->
      ({ model | counter = model.counter - 1}, Cmd.none)
    GotBook x ->
      ({model | bookState = Finished x}, Cmd.none)
    GotItems _ -> (model, Cmd.none)
    -- TODO: implementation
    GotEconomy v -> (model, Cmd.none)
    Reset ->
      (initialState, initialCmd)

btnFetchBook : Html Msg
btnFetchBook = button [ onClick GetBook ] [ text "GetBook" ]

viewBookState : BookState -> Html Msg
viewBookState bs =
  case bs of
      NotStarted -> btnFetchBook
      Started -> div [] []
      Finished (Ok t) ->
        div [] [  btnFetchBook
                  , text t ]
      Finished (Err e) -> div [ class "error"][ text (errorToString e)] -- https://elmprogramming.com/model-view-update-part-2.html


view : Model -> Html Msg
view model =
  div []
    [ div [] [ text (getBookStateText model.bookState) ]
    , button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model.counter) ]
    , button [ onClick Increment ] [ text "+" ]
    , viewBookState model.bookState
    ]

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

getBook : Cmd Msg
getBook =
  Http.get
    { url = "https://elm-lang.org/assets/public-opinion.txt"
    , expect = Http.expectString GotBook
    }

fetchItems : Cmd Msg
fetchItems =
  Http.post
    { url = "https://example.com/items.json"
    , body = Http.emptyBody
    , expect = Http.expectJson GotItems (D.list (D.field "name" D.string))
    }

getBookStateText : BookState -> String
getBookStateText bs =
  case bs of
    NotStarted -> "Nothing"
    Started -> "Fetching..."
    -- Finished (Result.Ok t) -> String.join "," t
    Finished (Result.Ok _) -> "Finished"
    Finished (Result.Err e) -> errorToString e

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

-- https://package.elm-lang.org/packages/elm/json/latest/Json.Decode

decodeGoodInfo: Decoder GoodInfo
decodeGoodInfo =
  D.map2 GoodInfo
    (D.field "Cost" D.int)
    (D.field "SoldBy" D.string)

economyDecoder : Decoder (Dict.Dict String GoodInfo)
economyDecoder =
  D.dict decodeGoodInfo