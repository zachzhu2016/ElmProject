module Page.Blank exposing (view)

import Html exposing (Html)


view : { title : String, content : Html msg }
view =
    { title = "Blank"
    , content = Html.text ""
    }
