module Page.Home exposing (Model, Msg(..), Status(..), gridDecoder, init, subscriptions, toCart, toSession, update, view, viewBanner, viewFoods, viewPreview)

import Api
import Api.Endpoint as Endpoint exposing (Endpoint)
import Browser.Navigation as Nav
import Cart exposing (Cart)
import Food exposing (Food, Preview)
import Food.Slug as FoodSlug exposing (Slug)
import FoodGrid exposing (FoodGrid)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required)
import Route exposing (Route)
import Session exposing (Session)
import Task exposing (Task)



-- MODEL


type alias Model =
    { session : Session
    , cart : Cart

    -- loaded from the server
    , foodGrid : Status (FoodGrid (Food Preview))
    }


type Status a
    = Loading
    | LoadingSlowly
    | Loaded a
    | Failed



{-
   type FeedTab
       = ForYou
       | NewArrivals
       | Popular
-}


init : Cart -> Session -> ( Model, Cmd Msg )
init cart session =
    ( { session = session
      , cart = cart
      , foodGrid = Loading
      }
    , Api.get Endpoint.foods CompletedGridLoad gridDecoder
    )



--HTTP
-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Home"
    , content =
        div [ class "ui vertical center aligned segment" ]
            [ div [ class "ui vertical segment " ]
                [ viewBanner ]
            , div [ class "ui container vertical segment" ]
                -- [ div [ class "ui secondary pointing menu " ] <|
                --   viewTabs model.feedTab,
                [ div [ class "ui container" ]
                    [ div [ class "ui four doubling link cards" ] <|
                        case model.foodGrid of
                            Loaded foodGrid ->
                                viewFoods foodGrid

                            Loading ->
                                [ text "Loading" ]

                            LoadingSlowly ->
                                [ text "Loading slowly" ]

                            Failed ->
                                [ text "Failed" ]
                    ]
                ]
            ]
    }



{-
   viewTabs : FeedTab -> List (Html Msg)
   viewTabs activeTab =
       [ navbarLink activeTab ForYou [ text "For You" ]
       , navbarLink activeTab NewArrivals [ text "New Arrivals" ]
       , navbarLink activeTab Popular [ text "Popular" ]
       ]


   navbarLink : FeedTab -> FeedTab -> List (Html Msg) -> Html Msg
   navbarLink activeTab tab linkContent =
       a
           [ classList [ ( "item", True ), ( "active", isActive activeTab tab ) ]
           , onClick (ClickedTab tab)
           , href ""
           ]
       <|
           linkContent


   isActive : FeedTab -> FeedTab -> Bool
   isActive activeTab tab =
       if activeTab == tab then
           True

       else
           False
-}


viewBanner : Html msg
viewBanner =
    div [ class "ui text container" ]
        [ h1 [ class "ui header" ] [ text "neora one" ]
        , div
            [ class "ui large primary button" ]
            [ text "Preorder now" ]
        ]


viewFoods : FoodGrid (Food Preview) -> List (Html msg)
viewFoods foodGrid =
    FoodGrid.values foodGrid
        |> List.map viewPreview


viewPreview : Food Preview -> Html msg
viewPreview food =
    let
        slug =
            Food.slug

        name =
            (Food.metadata food).name

        price =
            (Food.metadata food).price
    in
    div [ class "ui raised card" ]
        [ a [ class "square image", Route.href (Route.Food (Food.slug food)) ]
            {-
               [ div [ class "ui bottom right attached big label" ]
                   [ i [ class "dollar sign icon" ]
                       []
                   , text "4.99"
                   ]
            -}
            [ img
                [ src "../../../asset/images/shangwei1.jpg" ]
                []
            ]
        , div
            [ class "ui content" ]
            [ div [ class "header" ]
                [ text name ]
            ]
        , div
            [ class "ui compact primary button" ]
            [ i [ class "shop icon" ]
                []
            , text "Add To Cart"
            ]
        ]



-- UPDATE


type Msg
    = GotSession Session
    | CompletedGridLoad (Result Http.Error (FoodGrid (Food Preview)))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession session ->
            ( { model | session = session }, Cmd.none )

        CompletedGridLoad (Ok foodGrid) ->
            ( { model | foodGrid = Loaded foodGrid }, Cmd.none )

        CompletedGridLoad (Err error) ->
            ( { model | foodGrid = Failed }, Cmd.none )



-- SERIALIZATION


gridDecoder : Decoder (FoodGrid (Food Preview))
gridDecoder =
    Decode.succeed FoodGrid.fromList
        |> required "foods" (Decode.list Food.previewDecoder)



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
