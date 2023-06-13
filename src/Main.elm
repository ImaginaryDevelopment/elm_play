module Main exposing (..)

-- import Html exposing (Html, button, div, text, ul, li, a)

import Avorion
import Book
import Browser
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D exposing (Decoder)
import Json.Encode as Encode
import Shared exposing (..)



-- import Json.Encode
-- chaining https://allo-media.net/en/tech/learning/elm/2018/02/05/chaining-http-requests-in-elm.html


type AppSection
    = CounterSect
    | BookSect
    | AvorSect


type alias Model =
    { counter : Int
    , display : AppSection
    , bookModel : Book.Model
    , avorionModel : Avorion.Model
    }


type MenuMsg
    = GoCounter
    | GoBook
    | GoAvorion


type CounterMsg
    = Increment
    | Decrement


type Msg
    = MenuMsg MenuMsg
    | BookMsg Book.Msg
    | CounterMsg CounterMsg
    | AvorionMsg Avorion.Msg
    | Reset


initialState =
    { counter = 0, bookModel = Book.init, display = CounterSect, avorionModel = Nothing }


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialState, Cmd.map AvorionMsg Avorion.initCmd )


updateMenu : MenuMsg -> Model -> ( Model, Cmd Msg )
updateMenu msg model =
    case msg of
        GoCounter ->
            ( { model | display = CounterSect }, Cmd.none )

        GoBook ->
            ( { model | display = BookSect }, Cmd.none )

        GoAvorion ->
            ( { model | display = AvorSect }, Cmd.none )


updateCounter : CounterMsg -> Model -> ( Model, Cmd Msg )
updateCounter msg model =
    case msg of
        Increment ->
            ( { model | counter = model.counter + 1 }, Cmd.none )

        Decrement ->
            ( { model | counter = model.counter - 1 }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MenuMsg mMsg ->
            updateMenu mMsg model

        CounterMsg cMsg ->
            updateCounter cMsg model

        BookMsg bMsg ->
            let
                ( bModel, bCmd ) =
                    Book.update bMsg model.bookModel
            in
            ( { model | bookModel = bModel }, Cmd.map BookMsg bCmd )

        AvorionMsg aMsg ->
            let
                ( aModel, aCmd ) =
                    Avorion.update aMsg model.avorionModel
            in
            ( { model | avorionModel = aModel }, Cmd.map AvorionMsg aCmd )

        Reset ->
            ( initialState, Cmd.map AvorionMsg Avorion.initCmd )


createMenu isSelected msg title =
    if isSelected then
        a [ href "#" ] [ text title ]

    else
        a [ href "#", onClick (MenuMsg msg) ] [ text title ]


viewMenu : Model -> Html Msg
viewMenu model =
    div []
        [ ul []
            [ -- li [] [ a [href "#", onClick (MenuMsg GoCounter)][text "Counter"]]
              li [] [ createMenu (model.display == CounterSect) GoCounter "Counter" ]
            , li [] [ createMenu (model.display == BookSect) GoBook "Book" ]
            , li [] [ createMenu (model.display == AvorSect) GoAvorion "Avorion" ]
            ]
        ]


viewCounter : Model -> Html Msg
viewCounter model =
    div []
        [ button [ onClick (CounterMsg Decrement) ] [ text "-" ]
        , div [] [ text (String.fromInt model.counter) ]
        , button [ onClick (CounterMsg Increment) ] [ text "+" ]
        ]


view : Model -> Html Msg
view model =
    let
        subv =
            case model.display of
                BookSect ->
                    let
                        v =
                            Book.view model.bookModel
                    in
                    Html.map BookMsg v

                CounterSect ->
                    viewCounter model

                AvorSect ->
                    let
                        v =
                            Avorion.view model.avorionModel
                    in
                    Html.map AvorionMsg v
    in
    div []
        [ viewMenu model
        , subv
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
