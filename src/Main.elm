module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D

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

type Msg
  = GetBook
  | GotBook (Result Http.Error String)
  | GotItems (Result Http.Error (List String))
  | Increment
  | Decrement
  | Reset
initialState = { counter = 0, bookState = NotStarted, gotItems = Nothing }
initialCmd = Http.get
      { url = "https://elm-lang.org/assets/public-opinion.txt"
      , expect = Http.expectString GotBook
      }

main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }

init : () -> (Model, Cmd Msg)
init _ = ( initialState, initialCmd )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetBook ->
      Debug.log "Getting book!"
      ({model | bookState = Started}, getBook)
    Increment ->
      ({ model | counter = model.counter + 1 }, Cmd.none)
    Decrement ->
      ({ model | counter = model.counter - 1}, Cmd.none)
    GotBook x ->
      ({model | bookState = Finished x}, Cmd.none)
    GotItems _ -> (model, Cmd.none)
    Reset ->
      (initialState, initialCmd)

viewBookState : BookState -> Html Msg
viewBookState bs =
  case bs of
      NotStarted -> button [ onClick GetBook ] [ text "GetBook" ]
      Started -> div [] []
      Finished (Ok t) -> div [] [text t]
      Finished (Err e) -> div [ class "error"][ text (errorToString e)]


view : Model -> Html Msg
view model =
  div []
    [ div [] [ text (getBookStateText model.bookState) ]
    , button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model.counter) ]
    , button [ onClick Increment ] [ text "+" ]

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
    Finished (Result.Ok t) -> t
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