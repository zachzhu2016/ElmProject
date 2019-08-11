module Skeleton exposing (Page(..), view, viewFooter, viewHeader)

import Api
import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Route exposing (Route)
import Viewer exposing (Viewer)


type Page
    = Other
    | Home
    | Login
    | Register
    | Cart


view : Maybe Viewer -> Page -> { title : String, content : Html msg } -> Document msg
view maybeViewer page { title, content } =
    { title = title ++ " - Neora"
    , body =
        [ div [ class "ui container" ] <|
            viewHeader maybeViewer page
                :: content
                :: [ viewFooter ]
        ]
    }


viewHeader : Maybe Viewer -> Page -> Html msg
viewHeader maybeViewer page =
    let
        rightMenu =
            case maybeViewer of
                Just viewer ->
                    [ userRightMenu ]

                Nothing ->
                    [ userRightMenu ]

        --[ guestRightMenu ]
    in
    div [ class "ui header secondary menu" ] <|
        div [ class "item" ]
            [ a [ class "my-logo", Route.href Route.Home ] [ text "neora" ]
            ]
            :: rightMenu


guestRightMenu : Html msg
guestRightMenu =
    div [ class "right item" ]
        [ div [ class "ui compact buttons " ]
            [ a [ class "ui left primary button", Route.href Route.Register ]
                [ text "Sign up" ]
            , a [ class "ui right button", Route.href Route.Login ]
                [ text "Log in" ]
            ]
        ]


userRightMenu : Html msg
userRightMenu =
    div [ class "right item" ]
        [ div [ class "my-dropdown" ]
            [ a [ class "ui dropdown label", href "" ]
                [ i [ class "user icon" ]
                    []
                , text "profile"
                , div [ class "my-dropdown-content menu" ]
                    [ a [ class "item", href "/profile" ]
                        [ text "My Account" ]
                    , a [ class "item", Route.href Route.Logout ]
                        [ text "Log Out" ]
                    ]
                ]
            ]
        , div [ class "my-dropdown" ]
            [ a [ class "ui dropdown label", href "/cart" ]
                [ i [ class "shop icon" ] [], text "Cart" ]
            ]
        ]


viewFooter : Html msg
viewFooter =
    div [ class "ui vertical segment footer" ]
        [ div [ class "ui section divider" ] []
        , div [ class "ui center aligned container" ]
            [ div [ class "ui stackable centered grid" ]
                [ div [ class "four wide center aligned column" ]
                    [ h4 [ class "ui header" ]
                        [ text "Group 1" ]
                    , div [ class "ui link list" ]
                        [ a [ class "item", href "#" ]
                            [ text "Link One" ]
                        , a [ class "item", href "#" ]
                            [ text "Link Two" ]
                        , a [ class "item", href "#" ]
                            [ text "Link Three" ]
                        , a [ class "item", href "#" ]
                            [ text "Link Four" ]
                        ]
                    ]
                , div [ class "four wide center aligned column" ]
                    [ h4 [ class "ui header" ]
                        [ text "Group 2" ]
                    , div [ class "ui link list" ]
                        [ a [ class "item", href "#" ]
                            [ text "Link One" ]
                        , a [ class "item", href "#" ]
                            [ text "Link Two" ]
                        , a [ class "item", href "#" ]
                            [ text "Link Three" ]
                        , a [ class "item", href "#" ]
                            [ text "Link Four" ]
                        ]
                    ]
                , div [ class "four wide center aligned column" ]
                    [ h4 [ class "ui header" ]
                        [ text "Group 3" ]
                    , div [ class "ui link list" ]
                        [ a [ class "item", href "#" ]
                            [ text "Link One" ]
                        , a [ class "item", href "#" ]
                            [ text "Link Two" ]
                        , a [ class "item", href "#" ]
                            [ text "Link Three" ]
                        , a [ class "item", href "#" ]
                            [ text "Link Four" ]
                        ]
                    ]
                ]
            , div [ class "ui section divider" ]
                []

            --, img [ class "ui centered mini image", src "assets/images/logo.png" ]
            --    []
            , div [ class "ui horizontal  small divided link list" ]
                [ a [ class "item", href "#" ]
                    [ text "Site Map" ]
                , a [ class "item", href "#" ]
                    [ text "Contact Us" ]
                , a [ class "item", href "#" ]
                    [ text "Terms and Conditions" ]
                , a [ class "item", href "#" ]
                    [ text "Privacy Policy" ]
                ]
            ]
        ]
