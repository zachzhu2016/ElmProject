module Route exposing (Route(..), fromUrl, href, parser, replaceUrl, routeToString)

import Browser.Navigation as Nav
import Food.Slug as Slug exposing (Slug)
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)



-- ROUTING


type Route
    = Home
    | Root
    | Login
    | Logout
    | Food Slug
    | Cart
    | Register


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Login (s "login")
        , Parser.map Logout (s "logout")
        , Parser.map Food (s "food" </> Slug.urlParser)
        , Parser.map Register (s "register")
        , Parser.map Cart (s "cart")
        ]



-- PUBLIC HELPERS


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url



-- INTERNAL


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    []

                Root ->
                    []

                Login ->
                    [ "login" ]

                Logout ->
                    [ "logout" ]

                Food slug ->
                    [ "food", Slug.toString slug ]

                Register ->
                    [ "register" ]

                Cart ->
                    [ "cart" ]
    in
    "/" ++ String.join "/" pieces
