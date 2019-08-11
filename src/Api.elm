port module Api exposing (application, cartChanges, decodeFromChange, encodeItem, get, logout, onCartChange, onStoreChange, post, storeCache, storeCart, storeViewer, userUnwrap, viewerChanges)

import Api.Endpoint as Endpoint exposing (Endpoint)
import Browser
import Browser.Navigation as Nav
import Cart exposing (Cart)
import Dict exposing (Dict)
import Http exposing (Body, Error, Expect)
import Json.Decode as Decode exposing (Decoder, Value, andThen, decodeString, fail, field, maybe, string)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode
import Url exposing (Url)
import Viewer exposing (Viewer)



-- USER PERSISTENCE


storeViewer : Viewer -> Cmd msg
storeViewer (Viewer.Viewer uid tok eml cus) =
    let
        json =
            Encode.object
                [ ( "user"
                  , Encode.object
                        [ ( "id", Encode.int uid )
                        , ( "token", Encode.string tok )
                        , ( "email", Encode.string eml )
                        , ( "customer", Encode.int cus )
                        ]
                  )
                ]
    in
    storeCache (Just json)


logout : Cmd msg
logout =
    storeCache Nothing


port storeCache : Maybe Value -> Cmd msg


port onStoreChange : (Value -> msg) -> Sub msg


viewerChanges : (Maybe Viewer -> msg) -> Decoder Viewer -> Sub msg
viewerChanges toMsg decoder =
    onStoreChange (\value -> toMsg (decodeFromChange decoder value))


decodeFromChange : Decoder Viewer -> Value -> Maybe Viewer
decodeFromChange viewerDecoder val =
    -- It's stored in localStorage as a JSON String;
    -- first decode the Value as a String, then
    -- decode that String as JSON.
    Decode.decodeValue (userUnwrap viewerDecoder) val
        |> Result.toMaybe


userUnwrap : Decoder Viewer -> Decoder Viewer
userUnwrap viewerDecoder =
    Decode.field "user" viewerDecoder



-- CART PERSISTENCE


port storeCart : Value -> Cmd msg


port onCartChange : (Value -> msg) -> Sub msg


encodeItem : Cart.Item -> Value
encodeItem item =
    Encode.object
        [ ( "id", Encode.string item.id )
        , ( "name", Encode.string item.name )
        , ( "price", Encode.int item.price )
        , ( "qty", Encode.int item.qty )
        ]


cartChanges : (Maybe Cart -> msg) -> Decoder Cart -> Sub msg
cartChanges toMsg cartDecoder =
    onCartChange
        (\value ->
            toMsg
                --(Decode.decodeValue (cartUnwrap cartDecoder) value
                (Decode.decodeValue cartDecoder value
                    |> Result.toMaybe
                )
        )



{-

   cartUnwrap : Decoder Cart -> Decoder Cart
   cartUnwrap viewerDecoder =
       Decode.field "cart" viewerDecoder

-}
{-
   storeCart : Dict String Cart.Item -> Cmd msg
   storeCart dict =
       let
           json =
               Encode.dict identity encodeItem dict
       in
       storeCart json
-}
-- HTTP


get : Endpoint -> (Result Error a -> msg) -> Decoder a -> Cmd msg
get url toResult decoder =
    Endpoint.request
        { method = "GET"
        , url = url
        , expect = Http.expectJson toResult decoder
        , headers = []
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


post : Endpoint -> (Result Error a -> msg) -> Body -> Decoder a -> Cmd msg
post url toResult body decoder =
    Endpoint.request
        { method = "POST"
        , url = url
        , expect = Http.expectJson toResult decoder
        , headers = []
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        }



-- APPLICATION


application :
    Decoder Viewer
    -> Decoder Cart
    ->
        { init : Maybe Cart -> Maybe Viewer -> Url -> Nav.Key -> ( model, Cmd msg )
        , onUrlChange : Url -> msg
        , onUrlRequest : Browser.UrlRequest -> msg
        , subscriptions : model -> Sub msg
        , update : msg -> model -> ( model, Cmd msg )
        , view : model -> Browser.Document msg
        }
    -> Program Value model msg
application viewerDecoder cartDecoder config =
    let
        init flags url navKey =
            let
                maybeViewer =
                    Decode.decodeValue Decode.string flags
                        |> Result.andThen (Decode.decodeString (userUnwrap viewerDecoder))
                        |> Result.toMaybe

                maybeCart =
                    Decode.decodeValue Decode.string flags
                        --|> Result.andThen (Decode.decodeString (cartUnwrap cartDecoder))
                        |> Result.andThen (Decode.decodeString cartDecoder)
                        |> Result.toMaybe
            in
            config.init maybeCart maybeViewer url navKey
    in
    Browser.application
        { init = init
        , onUrlChange = config.onUrlChange
        , onUrlRequest = config.onUrlRequest
        , subscriptions = config.subscriptions
        , update = config.update
        , view = config.view
        }
