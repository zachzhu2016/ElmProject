module Page.Food exposing (Model, Msg(..), Status(..), init, subscriptions, toCart, toSession, update, view)

import Api
import Api.Endpoint as Endpoint
import Cart exposing (Cart)
import Dict exposing (Dict)
import Food exposing (Food, Full, Preview, fullDecoder)
import Food.Slug as Slug exposing (Slug)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Session exposing (Session)



--MODEL


type alias Model =
    { session : Session
    , cart : Cart
    , errors : List String
    , food : Status (Food Full)
    }


type Status a
    = Loading
    | LoadingSlowly
    | Loaded a
    | Failed


init : Cart -> Session -> Slug -> ( Model, Cmd Msg )
init cart session slug =
    ( { session = session
      , cart = cart
      , errors = []
      , food = Loading
      }
    , Api.get (Endpoint.food slug) CompletedLoadFood fullDecoder
    )



-- VIEW


view : Model -> { title : String, content : Html msg }
view model =
    case model.food of
        Loaded food ->
            let
                id =
                    (Food.metadata food).id

                name =
                    (Food.metadata food).name

                price =
                    (Food.metadata food).price

                restaurant =
                    (Food.metadata food).restaurant

                description =
                    (Food.metadata food).description

                slug =
                    Food.slug food
            in
            { title = name
            , content =
                div []
                    [ text (String.fromInt id)
                    , text name
                    , text (String.fromInt price)
                    , text restaurant
                    , text description
                    ]
            }

        Loading ->
            { title = "Food", content = text "loading" }

        LoadingSlowly ->
            { title = "Food", content = text "loading slowly" }

        Failed ->
            { title = "Food", content = text "failed" }



-- UPDATE


type Msg
    = GotSession Session
    | CompletedLoadFood (Result Http.Error (Food Full))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession session ->
            ( { model | session = session }, Cmd.none )

        CompletedLoadFood (Ok food) ->
            ( { model | food = Loaded food }, Cmd.none )

        CompletedLoadFood (Err error) ->
            ( { model | food = Failed }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session


toCart : Model -> Cart
toCart model =
    model.cart
