module Book exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D
import Shared exposing (..)


type Msg
    = GetBook
    | GotBook (HttpResult String)
    | GotItems (HttpResult (List String))


type alias Model =
    { bookState : BookState

    -- , gettingBook : Bool
    -- , gotBookResult : Maybe (Result Http.Error String)
    , gotItems : HttpState (List String)
    }


type BookState
    = NotStarted
    | Started
    | Finished (Result Http.Error String)


init : Model
init =
    { bookState = NotStarted
    , gotItems = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetBook ->
            Debug.log "Getting book!"
                -- https://riptutorial.com/elm/topic/2845/debugging
                ( { model | bookState = Started }, getBook )

        GotBook x ->
            ( { model | bookState = Finished x }, Cmd.none )

        GotItems _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text (getBookStateText model.bookState) ]
        , viewBookState model.bookState
        ]


getBook : Cmd Msg
getBook =
    Http.get
        { url = "https://elm-lang.org/assets/public-opinion.txt"
        , expect = Http.expectString GotBook
        }


fetchItems : Cmd Msg
fetchItems =
    -- currently unused
    Http.post
        { url = "https://example.com/items.json"
        , body = Http.emptyBody
        , expect = Http.expectJson GotItems (D.list (D.field "name" D.string))
        }


getBookStateText : BookState -> String
getBookStateText bs =
    case bs of
        NotStarted ->
            "Nothing"

        Started ->
            "Fetching..."

        -- Finished (Result.Ok t) -> String.join "," t
        Finished (Result.Ok _) ->
            "Finished"

        Finished (Result.Err e) ->
            errorToString e


btnFetchBook : Html Msg
btnFetchBook =
    button [ onClick GetBook ] [ text "GetBook" ]


viewBookState : BookState -> Html Msg
viewBookState bs =
    case bs of
        NotStarted ->
            btnFetchBook

        Started ->
            div [] []

        Finished (Ok t) ->
            div []
                [ btnFetchBook
                , text t
                ]

        Finished (Err e) ->
            div [ class "error" ] [ text (errorToString e) ]



-- https://elmprogramming.com/model-view-update-part-2.html
