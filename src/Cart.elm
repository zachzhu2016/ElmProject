module Cart exposing (Cart(..), Item, decoder, itemDecoder)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (optional, required)


type Cart
    = Empty
    | Filled (Dict String Item)


type alias Item =
    { id : String
    , name : String
    , price : Int
    , qty : Int

    --, restaurant : String
    --, description : String
    }



-- DECODE


decoder : Decoder Cart
decoder =
    Decode.map Filled dictDecoder


dictDecoder : Decoder (Dict String Item)
dictDecoder =
    Decode.dict itemDecoder


itemDecoder : Decoder Item
itemDecoder =
    Decode.succeed Item
        |> required "id" Decode.string
        |> required "name" Decode.string
        |> required "price" Decode.int
        |> required "qty" Decode.int
