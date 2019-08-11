module Page.Register exposing (Form, Model, Msg(..), Problem(..), TrimmedForm(..), ValidatedField(..), fieldsToValidate, init, register, subscriptions, toCart, toSession, trimFields, update, updateForm, validate, validateField, view, viewError, viewForm, viewProblem)

import Api
import Api.Endpoint as Endpoint exposing (Endpoint)
import Browser.Navigation as Nav
import Cart exposing (Cart)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
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
    , firstname : String
    , lastname : String
    , confirm : String
    }


init : Cart -> Session -> ( Model, Cmd msg )
init cart session =
    ( { session = session
      , cart = cart
      , problems = []
      , form =
            { email = ""
            , password = ""
            , firstname = ""
            , lastname = ""
            , confirm = ""
            }
      }
    , Cmd.none
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Register"
    , content =
        div [ class "ui vertical segment" ]
            [ div [ class "ui middle aligned center aligned grid" ]
                [ div [ class "six wide column" ]
                    [ h1 [] [ text "Sign up" ]
                    , viewError model.problems
                    , viewForm model.form
                    , div [ class "ui message" ] [ text "Have an account? ", a [ href "/login" ] [ text "Log in" ] ]
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
                [ input [ type_ "text", onInput EnteredFirst, value form.firstname, placeholder "First name" ] []
                ]
            ]
        , div [ class "field" ]
            [ div [ class "ui input" ]
                [ input [ type_ "text", onInput EnteredLast, value form.lastname, placeholder "Last name" ] []
                ]
            ]
        , div [ class "field" ]
            [ div [ class "ui input" ]
                [ input [ type_ "text", onInput EnteredEmail, value form.email, placeholder "Email" ] []
                ]
            ]
        , div [ class "field" ]
            [ div [ class "ui input" ]
                [ input [ type_ "text", onInput EnteredPass, value form.password, placeholder "Password" ] []
                ]
            ]
        , div [ class "field" ]
            [ div [ class "ui input" ]
                [ input [ type_ "text", onInput ConfirmedPass, value form.confirm, placeholder "Confirm Password" ] []
                ]
            ]
        , button [ class "ui fluid large submit primary button" ] [ text "Register" ]
        ]



-- UPDATE


type Msg
    = GotSession Session
    | SubmittedForm
    | EnteredFirst String
    | EnteredLast String
    | EnteredEmail String
    | EnteredPass String
    | ConfirmedPass String
    | CompletedRegister (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmittedForm ->
            case validate model.form of
                Ok validForm ->
                    ( { model | problems = [] }
                    , register validForm
                      -- http request to login API
                      -- login validForm
                    )

                Err problems ->
                    ( { model | problems = problems }
                    , Cmd.none
                    )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

        EnteredFirst firstname ->
            updateForm (\form -> { form | firstname = firstname }) model

        EnteredLast lastname ->
            updateForm (\form -> { form | lastname = lastname }) model

        EnteredEmail email ->
            updateForm (\form -> { form | email = email }) model

        EnteredPass password ->
            updateForm (\form -> { form | password = password }) model

        ConfirmedPass confirm ->
            updateForm (\form -> { form | confirm = confirm }) model

        CompletedRegister (Err error) ->
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

        CompletedRegister (Ok email) ->
            ( model
            , Route.replaceUrl (Session.navKey model.session) Route.Login
            )


updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- FORM


type TrimmedForm
    = Trimmed Form


type ValidatedField
    = Firstname
    | Lastname
    | Email
    | Password
    | Confirm


fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Firstname
    , Lastname
    , Email
    , Password
    , Confirm
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
            Firstname ->
                if String.isEmpty form.firstname then
                    [ "firstname can't be blank." ]

                else
                    []

            Lastname ->
                if String.isEmpty form.lastname then
                    [ "lastname can't be blank." ]

                else
                    []

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

            Confirm ->
                if String.isEmpty form.confirm then
                    [ "confirm password can't be blank." ]

                else if not (form.password == form.confirm) then
                    [ "password does not match" ]

                else
                    []



{--- future: check valid email
   isValidEmail : String -> Bool
   isValidEmail email =
           Regex.contains validEmail email

   validEmail : Regex
   validEmail =
           "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
                   |> Regex.fromStringWith { caseInsensitive = True, multiline = False }
                           |> Maybe.withDefault Regex.never

-}


trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { firstname = String.trim form.firstname
        , lastname = String.trim form.lastname
        , email = String.trim form.email
        , password = String.trim form.password
        , confirm = String.trim form.confirm
        }



-- HTTP


register : TrimmedForm -> Cmd Msg
register (Trimmed form) =
    let
        body =
            Http.stringBody
                "application/x-www-form-urlencoded"
                (String.concat
                    [ "fname=", form.firstname, "&lname=", form.lastname, "&email=", form.email, "&userpass=", form.password ]
                )
    in
    Api.post Endpoint.register CompletedRegister body (Decode.field "email" Decode.string)



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session


toCart : Model -> Cart
toCart model =
    model.cart
