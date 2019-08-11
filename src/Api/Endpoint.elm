module Api.Endpoint exposing (Endpoint(..), food, foods, login, register, request, unwrap, url)

import Food.Slug as Slug exposing (Slug)
import Http
import Url.Builder exposing (QueryParameter)


{-| Http.request, except it takes an Endpoint instead of a Url.
-}
request :
    { body : Http.Body
    , expect : Http.Expect a
    , headers : List Http.Header
    , method : String
    , timeout : Maybe Float
    , url : Endpoint
    , tracker : Maybe String
    }
    -> Cmd a
request config =
    Http.request
        { body = config.body
        , expect = config.expect
        , headers = config.headers
        , method = config.method
        , timeout = config.timeout
        , tracker = config.tracker
        , url = unwrap config.url
        }



-- TYPES


type Endpoint
    = Endpoint String


unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str


url : List String -> Endpoint
url paths =
    Url.Builder.crossOrigin "https://api.neora.co" paths []
        |> Endpoint



-- ENDPOINTS


login : Endpoint
login =
    url [ "login" ]


register : Endpoint
register =
    url [ "register" ]


foods : Endpoint
foods =
    url [ "food" ]


food : Slug -> Endpoint
food slug =
    url [ "food", Slug.toString slug ]
