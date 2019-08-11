module Viewer exposing (Viewer(..), customer, decoder, email, token, userid)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)


type Viewer
    = Viewer Int String String Int


userid : Viewer -> Int
userid (Viewer val _ _ _) =
    val


token : Viewer -> String
token (Viewer _ val _ _) =
    val


email : Viewer -> String
email (Viewer _ _ val _) =
    val


customer : Viewer -> Int
customer (Viewer _ _ _ val) =
    val



-- DECODER


decoder : Decoder Viewer
decoder =
    Decode.succeed Viewer
        |> required "id" Decode.int
        |> required "token" Decode.string
        |> required "email" Decode.string
        |> optional "customer" Decode.int 0
