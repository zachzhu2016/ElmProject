module Food.Slug exposing (Slug(..), decoder, toString, urlParser)

import Json.Decode as Decode exposing (Decoder)
import Url.Parser exposing (Parser)



-- CREATE


urlParser : Parser (Slug -> a) a
urlParser =
    Url.Parser.custom "SLUG" (\str -> Just (Slug str))


decoder : Decoder Slug
decoder =
    Decode.map Slug Decode.string



-- TYPES


type Slug
    = Slug String


toString : Slug -> String
toString (Slug str) =
    str
