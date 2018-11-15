module View.Elements exposing (authHeader, authentication, bookContetnDiv, bookInfoDiv, booksDiv, createBookBody, emailInput, errorDisplay, loginBody, passwordAgain, passwordInput, signUpBody, userHeader)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models.Models as Models
import Navigation exposing (Location)
import Routes
import Update



-- VIEW


authHeader : Html Update.Msg
authHeader =
    header []
        [ nav [ class "cyan darken-1" ]
            [ div [ class "nav-wrapper container" ]
                [ ul [ class "right" ]
                    [ li [] [ a [ class "btn lime", href <| Routes.path Routes.LoginRoute ] [ text "Login" ] ]
                    , li [] [ a [ class "btn lime", href <| Routes.path Routes.SignUpRoute ] [ text "Sign Up" ] ]
                    ]
                ]
            ]
        ]


booksDiv : List Models.Book -> Html Update.Msg
booksDiv books =
    div [ class "container" ] [ List.map bookInfoDiv books |> div [ class "row" ] ]


bookInfoDiv : Models.Book -> Html Update.Msg
bookInfoDiv book =
    div [ class "col s12 m12 l12" ]
        [ div [ onClick <| Update.UpdateRoute <| Routes.BookDetailRoute book.id, class "card small hoverable grey lighten-4" ]
            [ div [ class "card-content" ]
                [ h2 [] [ text <| "ID " ++ book.id ++ ": " ++ book.title ]
                , p [] [ text ("price :" ++ Basics.toString book.price ++ "$") ]
                , p [] [ text ("author :" ++ book.author) ]
                , p [] [ text book.content ]
                ]
            ]
        ]


bookContetnDiv : Models.Book -> Html Update.Msg
bookContetnDiv book =
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "col l6 offset-l3" ]
                [ h1 [] [ text <| "ID " ++ book.id ++ ": " ++ book.title ]
                , div [] [ text (Basics.toString book.price) ]
                , div [] [ text book.author ]
                , div [] [ text book.content ]
                ]
            ]
        ]


userHeader : Models.User -> Html Update.Msg
userHeader user =
    header []
        [ nav [ class "cyan darken-1" ]
            [ div [ class "nav-wrapper container" ]
                [ a [ href <| Routes.path Routes.AddBookRoute, class "btn lime" ] [ text "New Book" ]
                , ul [ class "right" ]
                    [ li [] [ text user.email ]
                    , li [] [ a [ onClick Update.Logout, class "btn lime" ] [ text "Logout" ] ]
                    ]
                ]
            ]
        ]


createBookBody : Models.BookForm -> Html Update.Msg
createBookBody form =
    div [ class "container " ]
        [ div [ class "row" ]
            [ Html.form [ class "col s12 m8 offset-m2" ]
                [ div [ class "input-field" ]
                    [ input
                        [ placeholder "Add title"
                        , value form.title
                        , type_ "text"
                        , onInput (\title -> Update.OnInputBook { form | title = title })
                        ]
                        []
                    ]
                , div [ class "input-field" ]
                    [ input
                        [ placeholder "Add author"
                        , value form.author
                        , type_ "text"
                        , onInput (\author -> Update.OnInputBook { form | author = author })
                        ]
                        []
                    ]
                , div [ class "input-field" ]
                    [ input
                        [ placeholder "Add book content"
                        , value form.content
                        , type_ "text"
                        , onInput (\content -> Update.OnInputBook { form | content = content })
                        ]
                        []
                    ]
                , div [ class "input-field" ]
                    [ input
                        [ placeholder "Add price"
                        , value form.price
                        , type_ "text"
                        , onInput (\price -> Update.OnInputBook { form | price = price })
                        ]
                        []
                    ]
                , a [ onClick Update.AddBook, class "btn right lime" ] [ text "Create" ]
                ]
            ]
        ]


errorDisplay : err -> Html Update.Msg
errorDisplay err =
    div [ class "container" ] [ text <| Basics.toString err ]


emailInput : Models.SignupForm -> Html Update.Msg
emailInput form =
    div [ class "input-field" ]
        [ i [ class "material-icons prefix" ] [ text "email" ]
        , input
            [ placeholder "Email"
            , type_ "text"
            , value form.email
            , onInput (\email -> Update.OnInputSignup { form | email = email })
            ]
            []
        ]


passwordInput : Models.SignupForm -> Html Update.Msg
passwordInput form =
    div [ class "input-field" ]
        [ i [ class "material-icons prefix" ]
            [ text "lock" ]
        , input
            [ placeholder "Password"
            , value form.password
            , type_ "password"
            , onInput (\password -> Update.OnInputSignup { form | password = password })
            ]
            []
        ]


passwordAgain : Models.SignupForm -> Html Update.Msg
passwordAgain form =
    div [ class "input-field" ]
        [ i [ class "material-icons prefix" ] [ text "lock" ]
        , input
            [ placeholder "Password Again"
            , value form.passwordAgain
            , type_ "password"
            , onInput (\again -> Update.OnInputSignup { form | passwordAgain = again })
            ]
            []
        ]


authentication : List (Html Update.Msg) -> Html Update.Msg
authentication body =
    main_ [ class "container " ]
        [ div [ class "full-height row valign-wrapper" ]
            [ Html.form [ class "col s12 m4 offset-m4" ]
                body
            ]
        ]


loginBody : Models.SignupForm -> Html Update.Msg
loginBody form =
    authentication
        [ emailInput form
        , passwordInput form
        , a [ onClick Update.Login, class "btn right lime" ] [ text "Login" ]
        ]


signUpBody : Models.SignupForm -> Html Update.Msg
signUpBody form =
    authentication
        [ emailInput form
        , passwordInput form
        , passwordAgain form
        , a [ onClick Update.SignUp, class "btn right lime" ] [ text "Sign Up" ]
        ]
