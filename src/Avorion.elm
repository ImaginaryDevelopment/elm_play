module Avorion exposing (Msg, Model, initialCmd, update)

import Dict

import Json.Decode as D
import Json.Decode exposing (Decoder)
import Json.Encode as Encode
import Http

import Shared exposing (..)

type alias GoodInfo = {
  cost: Int
  , soldBy: Maybe String
  }

type Msg
  = GotEconomy (HttpResult (Dict.Dict String GoodInfo))
type alias Economy = Dict.Dict String GoodInfo

type alias Model = HttpState Economy

initialCmd : Cmd Msg
initialCmd =
  Http.get {
    url = "../json/economy.json" -- reactor assumes /src root
    , expect = Http.expectJson GotEconomy economyDecoder
      -- , expect = Http.expectJson GotItems (D.list (D.field "name" D.string))
  }

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotEconomy httpRes ->
      let _ = (dumpEconomy httpRes) in
      (Just httpRes, Cmd.none)

dumpEconomy : (HttpResult (Dict.Dict String GoodInfo)) -> ()
dumpEconomy httpRes =
  case httpRes of
      Result.Ok v ->
        Debug.log "Got economy!"
        ()
      Result.Err e ->
        Debug.log ( "Failed to get economy and/or deserialize:" ++ errorToString e)
        ()

encodeGoodInfo : GoodInfo -> Encode.Value
encodeGoodInfo gi =
  Encode.object
    [ ("cost", Encode.int gi.cost)
    , ("soldBy", gi.soldBy |> Maybe.withDefault "" |> Encode.string)
    ]
-- https://package.elm-lang.org/packages/elm/json/latest/Json.Decode

decodeGoodInfo: Decoder GoodInfo
decodeGoodInfo =
  D.map2 GoodInfo
    (D.field "Cost" D.int)
    (D.field "SoldBy" (D.maybe D.string))

economyDecoder : Decoder (Dict.Dict String GoodInfo)
economyDecoder =
  D.dict decodeGoodInfo