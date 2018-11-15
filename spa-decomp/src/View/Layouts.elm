module View.Layouts exposing (addBookLayout, error, login, makeLayout, readBookContent, signUp, storeLayout, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models.Models as Models
import Routes
import Update
import View.Elements as Elements


makeLayout : Html Update.Msg -> Html Update.Msg -> Html Update.Msg
makeLayout header body =
    div []
        [ header, body ]


storeLayout : Models.Model -> Html Update.Msg
storeLayout model =
    Maybe.map Elements.userHeader model.user
        |> Maybe.withDefault Elements.authHeader
        |> flip makeLayout (Elements.booksDiv model.books)


readBookContent : String -> Models.Model -> Html Update.Msg
readBookContent id model =
    case List.head <| List.filter (\b -> b.id == id) model.books of
        Just book ->
            Maybe.map Elements.userHeader model.user
                |> Maybe.withDefault Elements.authHeader
                |> flip makeLayout (Elements.bookContetnDiv book)

        Nothing ->
            error "ERR"


login : Models.Model -> Html Update.Msg
login model =
    Elements.loginBody model.signupForm


signUp : Models.Model -> Html Update.Msg
signUp model =
    Elements.signUpBody model.signupForm


addBookLayout : Models.Model -> Html Update.Msg
addBookLayout model =
    case model.user of
        Just user ->
            makeLayout (Elements.userHeader user) (Elements.createBookBody model.bookForm)

        Nothing ->
            error "404 Not Found"


error : String -> Html Update.Msg
error err =
    Elements.errorDisplay err


view : Models.Model -> Html Update.Msg
view model =
    case model.route of
        Routes.IndexRoute ->
            storeLayout model

        Routes.BookDetailRoute id ->
            readBookContent id model

        Routes.AddBookRoute ->
            addBookLayout model

        Routes.LoginRoute ->
            login model

        Routes.SignUpRoute ->
            signUp model

        Routes.ErrRoute ->
            error "404 Not Found"
