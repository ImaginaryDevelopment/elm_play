module Avorion exposing (Economy, Model, Msg, init, update, view)

import Dict
import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D exposing (Decoder)
import Json.Encode as Encode
import Shared exposing (..)
import UIHelpers


type alias GoodInfo =
    { cost : Int
    , soldBy : Maybe String
    }


type MenuMsg
    = GoRaw
    | GoTable
    | GoInteractive


type InteractiveMsg
    = GoodSelected String


type Msg
    = GotEconomy (HttpResult (Dict.Dict String GoodInfo))
    | MenuMsg MenuMsg
    | InteractiveMsg InteractiveMsg


type alias Economy =
    Dict.Dict String GoodInfo


type Display
    = Raw
    | Table
    | Interactive


type alias Model =
    { economy : HttpState Economy
    , display : Display
    , showDollar : Bool
    , selected : Maybe String
    , selections : Dict.Dict String Int
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { economy = Nothing, display = Raw, showDollar = True, selected = Nothing, selections = Dict.empty }, initCmd )


initCmd : Cmd Msg
initCmd =
    Http.get
        { url = "../json/economy.json" -- reactor assumes /src root
        , expect = Http.expectJson GotEconomy economyDecoder
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotEconomy httpRes ->
            let
                _ =
                    dumpEconomy httpRes
            in
            ( { model | economy = Just httpRes }, Cmd.none )

        MenuMsg GoInteractive ->
            ( { model | display = Interactive }, Cmd.none )

        MenuMsg GoTable ->
            ( { model | display = Table }, Cmd.none )

        MenuMsg GoRaw ->
            ( { model | display = Raw }, Cmd.none )

        InteractiveMsg (GoodSelected name) ->
            let
                next =
                    name |> maybeValueString |> Maybe.map (\v -> model.selections |> foldSertDict (+) v 1) |> Maybe.withDefault model.selections

                _ =
                    Debug.log "Good Selected"
            in
            ( { model | selections = next }, Cmd.none )


viewMenu : Display -> Html.Html Msg
viewMenu display =
    div []
        [ ul []
            [ -- li [] [ a [href "#", onClick (MenuMsg GoCounter)][text "Counter"]]
              li [] [ createMenu (display == Interactive) GoInteractive "Interactive" ]
            , li [] [ createMenu (display == Table) GoTable "Table" ]
            , li [] [ createMenu (display == Raw) GoRaw "Raw" ] -- ooo baby I like it raw - Shimmy Ya
            ]
        ]


viewSelections : Dict.Dict String ( GoodInfo, Int ) -> Html Msg
viewSelections selections =
    div []
        [ table []
            [ thead []
                [ tr []
                    [ th [] [ text "Good" ]
                    , th [] [ text "Count" ]
                    , th [] [ text "Total" ]
                    ]
                ]
            , tbody []
                (selections
                    |> Dict.toList
                    |> List.map
                        (\( k, ( gi, v ) ) ->
                            tr [ gi.soldBy |> Maybe.withDefault "" |> Html.Attributes.title ]
                                [ td [] [ text k ]
                                , td [] [ formatInt v |> text ]
                                , td [] [ formatMoney (v * gi.cost) |> text ]
                                ]
                        )
                )
            ]
        ]


viewBody : Model -> Economy -> Html.Html Msg
viewBody model e =
    case model.display of
        Raw ->
            pre []
                [ -- it appears there's no reflection in elm?
                  Encode.encode 0 (encodeEconomy e)
                    |> text
                ]

        Table ->
            table []
                [ thead []
                    [ tr []
                        [ th [] [ text "Name" ]
                        , th [] [ text "Cost" ]
                        , th [] [ text "Sold By" ]
                        ]
                    ]
                , tbody
                    []
                    (e
                        |> Dict.toList
                        |> List.map
                            (\( k, v ) ->
                                tr []
                                    [ td [] [ text k ]
                                    , td []
                                        [ text
                                            (if model.showDollar then
                                                formatMoney v.cost

                                             else
                                                formatInt v.cost
                                            )
                                        ]
                                    , td [] [ v.soldBy |> Maybe.withDefault "" |> text ]
                                    ]
                            )
                    )
                ]

        Interactive ->
            div []
                [ -- text box to filter select?
                  model.selected
                    |> Maybe.andThen
                        (\v ->
                            Dict.get v e
                                |> Maybe.map (\_ -> v)
                        )
                    |> (\v -> v)
                    |> UIHelpers.selectOfItems (e |> Dict.toList |> List.map Tuple.first) (\k -> GoodSelected k |> InteractiveMsg) (\k -> ( k, k ))
                ]


view : Model -> Html.Html Msg
view model =
    case model.economy of
        Nothing ->
            div [] [ text "No Economy loaded" ]

        Just (Err e) ->
            div [] [ errorToString e |> text ]

        Just (Ok ec) ->
            div []
                [ viewMenu model.display
                , viewBody model ec
                ]


dumpEconomy : HttpResult (Dict.Dict String GoodInfo) -> ()
dumpEconomy httpRes =
    case httpRes of
        Result.Ok _ ->
            Debug.log "Got economy!"
                ()

        Result.Err e ->
            Debug.log ("Failed to get economy and/or deserialize:" ++ errorToString e)
                ()


encodeGoodInfo : GoodInfo -> Encode.Value
encodeGoodInfo gi =
    Encode.object
        [ ( "cost", Encode.int gi.cost )
        , ( "soldBy", gi.soldBy |> Maybe.withDefault "" |> Encode.string )
        ]


encodeEconomy : Economy -> Encode.Value
encodeEconomy ec =
    -- https://package.elm-lang.org/packages/elm/json/latest/Json.Encode
    Encode.dict identity encodeGoodInfo ec



-- https://package.elm-lang.org/packages/elm/json/latest/Json.Decode


decodeGoodInfo : Decoder GoodInfo
decodeGoodInfo =
    D.map2 GoodInfo
        (D.field "Cost" D.int)
        (D.field "SoldBy" (D.maybe D.string))


economyDecoder : Decoder (Dict.Dict String GoodInfo)
economyDecoder =
    D.dict decodeGoodInfo


createMenu : Bool -> MenuMsg -> String -> Html Msg
createMenu isSelected msg title =
    UIHelpers.createMenu MenuMsg msg title isSelected
