module Main exposing (..)

import Dict

import Browser
-- import Html exposing (Html, button, div, text, ul, li, a)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D
import Json.Decode exposing (Decoder)
import Json.Encode as Encode

import Shared exposing (..)

import Avorion


-- import Json.Encode

-- chaining https://allo-media.net/en/tech/learning/elm/2018/02/05/chaining-http-requests-in-elm.html

type AppSection
  = CounterSect
  | BookSect
  | AvorSect

type BookState
  = NotStarted
  | Started
  | Finished (Result Http.Error String)


type alias GoodInfo = {
  cost: Int
  , soldBy: Maybe String
  }

encodeGoodInfo : GoodInfo -> Encode.Value
encodeGoodInfo gi =
  Encode.object
    [ ("cost", Encode.int gi.cost)
    , ("soldBy", gi.soldBy |> Maybe.withDefault "" |> Encode.string)
    ]
type alias Economy = Dict.Dict String GoodInfo

encodeEconomy : Economy -> Encode.Value
encodeEconomy ec = -- https://package.elm-lang.org/packages/elm/json/latest/Json.Encode
  Encode.dict identity encodeGoodInfo ec


type alias Model =
  { counter : Int
    , display: AppSection
    , bookState: BookState
    -- , gettingBook : Bool
    -- , gotBookResult : Maybe (Result Http.Error String)
    , gotItems: HttpState (List String)
    , economy: HttpState Economy
  }
type MenuMsg
  = GoCounter
  | GoBook
  | GoAvorion
type BookMsg
  = GetBook
  | GotBook (HttpResult String)
  | GotItems (HttpResult (List String))
type CounterMsg
  = Increment
  | Decrement

type Msg
  = MenuMsg MenuMsg
  | BookMsg BookMsg
  | CounterMsg CounterMsg
  | AvorionMsg Avorion.Msg
  | Reset

initialState = { counter = 0, bookState = NotStarted, display = CounterSect, gotItems = Nothing, economy = Nothing}

main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }

init : () -> (Model, Cmd Msg)
init _ = ( initialState,Cmd.map AvorionMsg Avorion.initialCmd )

updateMenu : MenuMsg -> Model -> (Model, Cmd Msg)
updateMenu msg model =
  case msg of
      GoCounter -> ({model | display = CounterSect}, Cmd.none)
      GoBook -> ({model | display = BookSect}, Cmd.none)
      GoAvorion -> ({model | display = AvorSect}, Cmd.none)
updateCounter : CounterMsg -> Model -> (Model, Cmd Msg)
updateCounter msg model =
  case msg of
    Increment ->
      ({ model | counter = model.counter + 1 }, Cmd.none)
    Decrement ->
      ({ model | counter = model.counter - 1}, Cmd.none)

updateBook : BookMsg -> Model -> (Model, Cmd Msg)
updateBook msg model =
  case msg of
    GetBook ->
      Debug.log "Getting book!" -- https://riptutorial.com/elm/topic/2845/debugging
      ({model | bookState = Started}, getBook)
    GotBook x ->
      ({model | bookState = Finished x}, Cmd.none)
    GotItems _ -> (model, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MenuMsg mMsg ->
      updateMenu mMsg model
    CounterMsg cMsg ->
      updateCounter cMsg model
    BookMsg bMsg ->
      updateBook bMsg model
    AvorionMsg aMsg ->
      let (aModel, aCmd) = Avorion.update aMsg model.economy
      in
      ({model | economy = aModel}, Cmd.map AvorionMsg aCmd )
    Reset ->
      (initialState, Cmd.map AvorionMsg Avorion.initialCmd)

btnFetchBook : Html Msg
btnFetchBook = button [ onClick (BookMsg GetBook) ] [ text "GetBook" ]

viewBookState : BookState -> Html Msg
viewBookState bs =
  case bs of
      NotStarted -> btnFetchBook
      Started -> div [] []
      Finished (Ok t) ->
        div [] [  btnFetchBook
                  , text t ]
      Finished (Err e) -> div [ class "error"][ text (errorToString e)] -- https://elmprogramming.com/model-view-update-part-2.html

createMenu isSelected msg title =
  if isSelected then
    a [href "#"] [text title]
  else
    a [href "#", onClick (MenuMsg msg)][text title]

viewMenu : Model -> Html Msg
viewMenu model =
  div [][
    ul [] [
      -- li [] [ a [href "#", onClick (MenuMsg GoCounter)][text "Counter"]]
      li [] [ createMenu (model.display == CounterSect) GoCounter "Counter" ]
      , li [] [ createMenu (model.display == BookSect) GoBook "Book"]
      , li [] [ createMenu (model.display == AvorSect) GoAvorion "Avorion"]
    ]
  ]

viewBook : Model -> Html Msg
viewBook model =
  div []
    [
      div [] [ text (getBookStateText model.bookState) ]
      , viewBookState model.bookState
    ]
viewCounter : Model -> Html Msg
viewCounter model =
  div [] [
      button [ onClick (CounterMsg Decrement) ] [ text "-" ]
      , div [] [ text (String.fromInt model.counter) ]
      , button [ onClick (CounterMsg Increment) ] [ text "+" ]
  ]

viewAvor : Model -> Html Msg
viewAvor model =
  case model.economy of
      Nothing ->
        div [] [ text "No Economy loaded"]
      Just (Err e) -> div [] [errorToString e |> text]
      Just (Ok ec) ->
        div [] [
          pre [] [ -- it appears there's no reflection in elm?
            Encode.encode 0 (encodeEconomy ec)
            |> text
          ]
        ]
view : Model -> Html Msg
view model =
  let
    subv =
      (case model.display of
        BookSect -> viewBook model
        CounterSect -> viewCounter model
        AvorSect -> viewAvor model
      )
  in
  div []
    [ viewMenu model
      , subv
    ]

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

getBook : Cmd Msg
getBook =
  Http.get
    { url = "https://elm-lang.org/assets/public-opinion.txt"
    , expect = Http.expectString (GotBook>>BookMsg)
    }

fetchItems : Cmd Msg
fetchItems =
  Http.post
    { url = "https://example.com/items.json"
    , body = Http.emptyBody
    , expect = Http.expectJson (GotItems >> BookMsg ) (D.list (D.field "name" D.string))
    }

getBookStateText : BookState -> String
getBookStateText bs =
  case bs of
    NotStarted -> "Nothing"
    Started -> "Fetching..."
    -- Finished (Result.Ok t) -> String.join "," t
    Finished (Result.Ok _) -> "Finished"
    Finished (Result.Err e) -> errorToString e

