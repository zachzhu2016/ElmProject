module Food exposing (Food(..), Full(..), Internals, Metadata, Preview(..), fullDecoder, metadata, metadataDecoder, previewDecoder, slug)

import Api.Endpoint as Endpoint exposing (Endpoint)
import Browser
import Browser.Navigation as Nav
import Food.Slug as Slug exposing (Slug)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (custom, hardcoded, required)


type Food a
    = Food Internals a


type Preview
    = Preview


type Full
    = Full


type alias Internals =
    { slug : Slug
    , metadata : Metadata
    }


type alias Metadata =
    { id : Int
    , name : String
    , restaurant : String
    , price : Int
    , description : String
    }



-- INFO


metadata : Food a -> Metadata
metadata (Food internals _) =
    internals.metadata


slug : Food a -> Slug
slug (Food internals _) =
    internals.slug



-- SERIALIZATION


previewDecoder : Decoder (Food Preview)
previewDecoder =
    Decode.succeed Food
        |> custom internalsDecoder
        |> hardcoded Preview


fullDecoder : Decoder (Food Full)
fullDecoder =
    Decode.succeed Food
        |> custom internalsDecoder
        |> hardcoded Full


internalsDecoder : Decoder Internals
internalsDecoder =
    Decode.succeed Internals
        |> required "slug" Slug.decoder
        |> custom metadataDecoder


metadataDecoder : Decoder Metadata
metadataDecoder =
    Decode.succeed Metadata
        |> required "id" Decode.int
        |> required "name" Decode.string
        |> required "restaurant" Decode.string
        |> required "price" Decode.int
        |> required "description" (Decode.map (Maybe.withDefault "") (Decode.nullable Decode.string))
