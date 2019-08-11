module Profile exposing (Address(..), Model)

import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , addressList : List Address

    --, orderList : OrderList
    }


type Address
    = Name String
    | Street String
    | City String
    | State String
    | Zip Int



-- VIEW
-- UPDATE
-- HTTP
