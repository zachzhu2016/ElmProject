module Page.Cart exposing (Model, Msg(..), calcTotal, init, subscriptions, toSession, update, view, viewCart, viewCartHeader, viewRow)

import Api
import Cart exposing (Cart)
import Debug
import Dict exposing (Dict)
import FoodGrid exposing (FoodGrid)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Encode as Encode
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , cart : Cart
    }


init : Session -> Cart -> ( Model, Cmd Msg )
init session cart =
    ( { session = session
      , cart = cart
      }
    , Cmd.none
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Cart"
    , content = div [ class "ui container" ] [ viewCart model.cart ]
    }



{-
   viewSteps : Html msg
   viewSteps =
       div [ class "ui three steps" ]
           [ div [ class "active step" ]
               [ i [ class "cart icon" ]
                   []
               , div [ class "content" ]
                   [ div [ class "title" ]
                       [ text "Shopping Cart" ]
                   ]
               ]
           , div [ class "disabled step" ]
               [ i [ class "payment icon" ]
                   []
               , div [ class "content" ]
                   [ div [ class "title" ]
                       [ text "Shipping & Billing" ]
                   ]
               ]
           , div [ class "disabled step" ]
               [ i [ class "info icon" ]
                   []
               , div [ class "content" ]
                   [ div [ class "title" ]
                       [ text "Confirm Order" ]
                   ]
               ]
           ]

-}


viewCart : Cart -> Html Msg
viewCart cart =
    case cart of
        Cart.Empty ->
            div [ class "ui center aligned container" ]
                [ h1 []
                    [ i [ class "shopping cart icon" ]
                        []
                    , text " Your shopping cart is empty"
                    ]
                ]

        Cart.Filled dict ->
            div [ class "container" ]
                [ viewCartHeader
                , table [ class "ui selectable unstackable basic padded large table" ]
                    [ thead []
                        [ tr []
                            [ th [ class "seven wide" ]
                                [ text "Item" ]
                            , th [ class "three wide" ]
                                [ text "Price" ]
                            , th [ class "three wide" ]
                                [ text "Quantity" ]
                            , th [ class "three wide" ]
                                [ text "Item Total" ]
                            ]
                        ]
                    , tbody [] <|
                        List.map viewRow (Dict.values dict)
                    , tfoot [ class "full-width" ]
                        [ tr []
                            [ th [ attribute "colspan" "4" ]
                                [ div [ class "ui big label" ]
                                    [ text "Cart Total"
                                    , div [ class "detail" ]
                                        [ text ("$" ++ String.fromFloat (calcTotal dict)) ]
                                    ]
                                , div [ class "ui right floated primary labeled icon large button" ]
                                    [ i [ class "check icon" ]
                                        []
                                    , text "Next Step"
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]


calcTotal : Dict String Cart.Item -> Float
calcTotal dict =
    let
        list =
            Dict.values dict
    in
    toFloat (List.sum (List.map (\item -> item.price * item.qty) list)) / 100


viewCartHeader : Html msg
viewCartHeader =
    h2 [ class "ui header" ]
        [ i [ class "cart icon" ]
            []
        , div [ class "content" ]
            [ text "Shopping Cart"

            {-
               , div [ class "sub header" ]
                   [ text "Manage your preferences" ]
            -}
            ]
        ]


viewRow : Cart.Item -> Html Msg
viewRow item =
    tr []
        [ td []
            [ text item.name ]
        , td []
            [ text (String.fromFloat (toFloat item.price / 100.0)) ]
        , td []
            [ div [ class "input" ]
                [ div [ class "ui mini input " ]
                    [ input
                        [ onInput (QtyChanged item.id)
                        , value (String.fromInt item.qty)
                        , Html.Attributes.min "1"
                        , Html.Attributes.max "50"
                        , Html.Attributes.name "quantity"
                        , type_ "number"
                        , Html.Attributes.style "width" "70px"
                        ]
                        []
                    ]
                ]
            ]
        , td []
            [ text ("$" ++ String.fromFloat (toFloat (item.price * item.qty) / 100.0)) ]
        ]



-- UPDATE


type
    Msg
    -- = RemovedCart.Item
    -- | IncrementedQty
    -- | DecrementedQty
    -- | ConfirmedCart
    = GotSession Session
    | QtyChanged String String
    | CartUpdated (Maybe Cart)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession session ->
            ( { model | session = session }, Cmd.none )

        QtyChanged id strQty ->
            let
                qty =
                    String.toInt strQty

                func =
                    Maybe.map (\item -> { item | qty = Maybe.withDefault 1 qty })

                newDict =
                    case model.cart of
                        Cart.Empty ->
                            Dict.empty

                        Cart.Filled dict ->
                            Dict.update id func dict

                json =
                    Encode.dict identity Api.encodeItem newDict
            in
            --( { model | cart = newCart }, storeCart json )
            ( model, Cmd.batch [ Api.storeCart json, Debug.log (Debug.toString json) Cmd.none ] )

        CartUpdated maybeCart ->
            ( { model | cart = Maybe.withDefault Cart.Empty maybeCart }, Cmd.none )



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Session.changes GotSession (Session.navKey model.session)
        , Api.cartChanges CartUpdated Cart.decoder
        ]



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
