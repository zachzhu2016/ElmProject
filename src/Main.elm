module Main exposing (Model(..), Msg(..), changeRouteTo, init, main, subscriptions, toSession, update, view)

import Api
import Browser exposing (Document)
import Browser.Navigation as Nav
import Cart exposing (Cart)
import Dict exposing (Dict)
import Html exposing (..)
import Json.Decode as Decode exposing (Value)
import Page.Blank as Blank
import Page.Cart as Cart
import Page.Food as Food
import Page.Home as Home
import Page.Login as Login
import Page.Register as Register
import Route exposing (Route)
import Session exposing (Session)
import Skeleton exposing (Page)
import Url exposing (Url)
import Viewer exposing (Viewer)



-- MAIN


main : Program Value Model Msg
main =
    Api.application
        Viewer.decoder
        Cart.decoder
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        }



-- MODEL


type Model
    = Redirect Session Cart
    | Home Home.Model
    | Login Login.Model
    | Register Register.Model
    | Food Food.Model
    | Cart Cart.Model
    | NotFound Session


init : Maybe Cart -> Maybe Viewer -> Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeCart maybeViewer url navKey =
    let
        cart =
            --Maybe.withDefault Cart.Empty maybeCart
            Cart.Filled (Dict.fromList [ ( "1", Cart.Item "1" "chicken" 599 5 ) ])
    in
    changeRouteTo (Route.fromUrl url)
        (Redirect (Session.fromViewer navKey maybeViewer) cart)



-- VIEW


view : Model -> Document Msg
view model =
    let
        viewer =
            Session.viewer (toSession model)

        viewPage page toMsg config =
            let
                { title, body } =
                    Skeleton.view viewer page config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        Redirect _ _ ->
            Skeleton.view viewer Skeleton.Other Blank.view

        NotFound _ ->
            Skeleton.view viewer Skeleton.Other Blank.view

        Home home ->
            viewPage Skeleton.Home GotHomeMsg (Home.view home)

        Register register ->
            viewPage Skeleton.Other GotRegisterMsg (Register.view register)

        Login login ->
            viewPage Skeleton.Other GotLoginMsg (Login.view login)

        Food food ->
            viewPage Skeleton.Other GotFoodMsg (Food.view food)

        Cart cart ->
            viewPage Skeleton.Other GotCartMsg (Cart.view cart)



-- UPDATE


type Msg
    = ChangedRoute (Maybe Route)
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotHomeMsg Home.Msg
    | GotLoginMsg Login.Msg
    | GotRegisterMsg Register.Msg
    | GotFoodMsg Food.Msg
    | GotCartMsg Cart.Msg
    | GotSession Session


toSession : Model -> Session
toSession page =
    case page of
        Redirect session _ ->
            session

        NotFound session ->
            session

        Home home ->
            Home.toSession home

        Login login ->
            Login.toSession login

        Register register ->
            Register.toSession register

        Food food ->
            Food.toSession food

        Cart cart ->
            Cart.toSession cart


toCart : Model -> Cart
toCart page =
    case page of
        Redirect _ cart ->
            cart

        NotFound _ ->
            Cart.Empty

        Home home ->
            Home.toCart home

        Login login ->
            Login.toCart login

        Register register ->
            Register.toCart register

        Food food ->
            Food.toCart food

        Cart cart ->
            cart.cart


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model

        cart =
            toCart model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Logout ->
            ( model, Api.logout )

        Just Route.Root ->
            ( model, Route.replaceUrl (Session.navKey session) Route.Home )

        Just Route.Home ->
            Home.init cart session
                |> updateWith Home GotHomeMsg model

        Just Route.Login ->
            Login.init cart session
                |> updateWith Login GotLoginMsg model

        Just Route.Register ->
            Register.init cart session
                |> updateWith Register GotRegisterMsg model

        Just (Route.Food slug) ->
            Food.init cart session slug
                |> updateWith Food GotFoodMsg model

        Just Route.Cart ->
            Cart.init session cart
                |> updateWith Cart GotCartMsg model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( ChangedRoute route, _ ) ->
            changeRouteTo route model

        ( GotHomeMsg subMsg, Home home ) ->
            Home.update subMsg home
                |> updateWith Home GotHomeMsg model

        ( GotLoginMsg subMsg, Login login ) ->
            Login.update subMsg login
                |> updateWith Login GotLoginMsg model

        ( GotRegisterMsg subMsg, Register register ) ->
            Register.update subMsg register
                |> updateWith Register GotRegisterMsg model

        ( GotFoodMsg subMsg, Food food ) ->
            Food.update subMsg food
                |> updateWith Food GotFoodMsg model

        ( GotCartMsg subMsg, Cart cart ) ->
            Cart.update subMsg cart
                |> updateWith Cart GotCartMsg model

        ( GotSession session, Redirect _ cart ) ->
            ( Redirect session cart
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Redirect _ _ ->
            Session.changes GotSession (Session.navKey (toSession model))

        Home home ->
            Sub.map GotHomeMsg (Home.subscriptions home)

        Login login ->
            Sub.map GotLoginMsg (Login.subscriptions login)

        Register register ->
            Sub.map GotRegisterMsg (Register.subscriptions register)

        Food food ->
            Sub.map GotFoodMsg (Food.subscriptions food)

        Cart cart ->
            Sub.map GotCartMsg (Cart.subscriptions cart)

        _ ->
            Sub.none
