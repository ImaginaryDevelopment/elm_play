module Main exposing (..)


import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D

-- type Model
--   = Failure
--   | Loading
--   | Success String

-- type alias Model =
--   { name : String
--   , password : String
--   , passwordAgain : String
--   }

type alias Model =
  { counter : Int
    , gotBookResult : Maybe (Result Http.Error String)
    , gotItems: Maybe (Result Http.Error (List String))
  }

type Msg
  = GotBook (Result Http.Error String)
  | GotItems (Result Http.Error (List String))
  | Increment
  | Decrement

main =
  -- let view2 msg =
  --   {title = "hello", body = [ view msg ] }
  -- in
  -- Browser.sandbox { init = 0, update = update, view = view }
    -- Browser.document {init = init, view = view2, update = update, subscriptions = ignore}
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }

init : () -> (Model, Cmd Msg)
init _ =
  ( { counter = 0, gotBookResult = Nothing, gotItems = Nothing }
  , Http.get
      { url = "https://elm-lang.org/assets/public-opinion.txt"
      , expect = Http.expectString GotBook
      }
  )


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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Increment ->
      ({ model | counter = model.counter + 1 }, Cmd.none)
    Decrement ->
      ({ model | counter = model.counter - 1}, Cmd.none)
    GotBook (Result.Ok b) -> (model, Cmd.none)
    GotBook (Result.Err e) ->(model, Cmd.none)
    GotItems _ -> (model, Cmd.none)

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model.counter) ]
    , button [ onClick Increment ] [ text "+" ]
    -- , button [ onClick ]
    ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
-- main =
--   Browser.element
--     { init = init
--     , update = update
--     , subscriptions = subscriptions
--     , view = view
--     }