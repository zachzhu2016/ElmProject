module Page.Login exposing (Form, Model, Msg(..), Problem(..), TrimmedForm(..), ValidatedField(..), fieldsToValidate, init, login, subscriptions, toCart, toSession, trimFields, update, updateForm, validate, validateField, view, viewError, viewForm, viewProblem)

import Api
import Api.Endpoint as Endpoint exposing (Endpoint)
import Browser.Navigation as Nav
import Cart exposing (Cart)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Route exposing (Route)
import Session exposing (Session)
import Viewer exposing (Viewer)



-- MODEL


type alias Model =
    { session : Session
    , cart : Cart
    , problems : List Problem
    , form : Form
    }


type Problem
    = InvalidEntry ValidatedField String
    | ServerError String


type alias Form =
    { email : String
    , password : String
    }


init : Cart -> Session -> ( Model, Cmd msg )
init cart session =
    ( { session = session
      , cart = cart
      , problems = []
      , form =
            { email = ""
            , password = ""
            }
      }
    , Cmd.none
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Login"
    , content =
        div [ class "ui vertical segment" ]
            [ div [ class "ui middle aligned center aligned grid" ]
                [ div [ class "six wide column" ]
                    [ h1 [] [ text "Sign in" ]
                    , viewError model.problems
                    , viewForm model.form
                    , div [ class "ui message" ] [ text "New to us? ", a [ href "/register" ] [ text "Sign Up" ] ]
                    ]
                ]
            ]
    }


viewError : List Problem -> Html msg
viewError problems =
    case problems of
        [] ->
            text ""

        _ ->
            div [ class "ui error message" ]
                [ ul [ class "list" ] (List.map viewProblem problems) ]


viewProblem : Problem -> Html msg
viewProblem problem =
    let
        errorMessage =
            case problem of
                InvalidEntry _ str ->
                    str

                ServerError str ->
                    str
    in
    li [] [ text errorMessage ]


viewForm : Form -> Html Msg
viewForm form =
    Html.form [ class "ui large form", onSubmit SubmittedForm ]
        [ div [ class "field" ]
            [ div [ class "ui input" ]
                [ input [ type_ "text", onInput EnteredEmail, value form.email, placeholder "Email" ] []
                ]
            ]
        , div [ class "field" ]
            [ div [ class "ui input" ]
                [ input [ type_ "text", onInput EnteredPassword, value form.password, placeholder "Password" ] []
                ]
            ]
        , button [ class "ui fluid large submit primary button" ] [ text "Login" ]
        ]



-- UPDATE


type Msg
    = SubmittedForm
    | GotSession Session
    | EnteredEmail String
    | EnteredPassword String
    | CompletedLogin (Result Http.Error Viewer)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmittedForm ->
            case validate model.form of
                Ok validForm ->
                    ( { model | problems = [] }
                    , login validForm
                      {-
                         , Api.sendRequest
                             (Encode.object
                                 [ ( "email", Encode.string validForm.email )
                                 , ( "userpass", Encode.string validForm.password )
                                 ]
                             )
                      -}
                      -- http request to login API
                    )

                Err problems ->
                    ( { model | problems = problems }
                    , Cmd.none
                    )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

        EnteredEmail email ->
            updateForm (\form -> { form | email = email }) model

        EnteredPassword password ->
            updateForm (\form -> { form | password = password }) model

        CompletedLogin (Err error) ->
            let
                errorString =
                    case error of
                        Http.BadUrl string ->
                            string

                        Http.Timeout ->
                            "Time out"

                        Http.NetworkError ->
                            "Network Error"

                        Http.BadStatus int ->
                            "Bad status: " ++ String.fromInt int

                        Http.BadBody string ->
                            "Bad body: " ++ string
            in
            ( { model | problems = List.append model.problems [ ServerError errorString ] }
            , Cmd.none
            )

        CompletedLogin (Ok viewer) ->
            ( model
            , Api.storeViewer viewer
            )


updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



--(Session.navKey model.session))
-- FORM


type TrimmedForm
    = Trimmed Form


type ValidatedField
    = Email
    | Password


fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Email
    , Password
    ]


validate : Form -> Result (List Problem) TrimmedForm
validate form =
    let
        trimmedForm =
            trimFields form
    in
    case List.concatMap (validateField trimmedForm) fieldsToValidate of
        [] ->
            Ok trimmedForm

        problems ->
            Err problems


validateField : TrimmedForm -> ValidatedField -> List Problem
validateField (Trimmed form) field =
    List.map (InvalidEntry field) <|
        case field of
            Email ->
                if String.isEmpty form.email then
                    [ "email can't be blank." ]

                else
                    []

            Password ->
                if String.isEmpty form.password then
                    [ "password can't be blank." ]

                else
                    []


trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { email = String.trim form.email
        , password = String.trim form.password
        }



-- HTTP


login : TrimmedForm -> Cmd Msg
login (Trimmed form) =
    let
        body =
            Http.stringBody
                "application/x-www-form-urlencoded"
                (String.concat
                    [ "email=", form.email, "&userpass=", form.password ]
                )
    in
    -- url + Message + message body + decoder
    Api.post Endpoint.login CompletedLogin body Viewer.decoder



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session


toCart : Model -> Cart
toCart model =
    model.cart
