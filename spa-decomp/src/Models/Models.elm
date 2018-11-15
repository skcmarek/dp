module Models.Models exposing (Book, BookForm, LoginForm, Model, SignupForm, User, createBook, initBook)

import Routes


type alias SignupForm =
    { email : String
    , password : String
    , passwordAgain : String
    }


type alias LoginForm =
    { email : String
    , password : String
    }


type alias BookForm =
    { title : String
    , author : String
    , price : String
    , content : String
    }


type alias Book =
    { id : String
    , title : String
    , price : Int
    , author : String
    , content : String
    }


type alias User =
    { email : String
    }


type alias Model =
    { books : List Book
    , user : Maybe User
    , route : Routes.Route
    , loginForm : LoginForm
    , signupForm : SignupForm
    , bookForm : BookForm
    }


initBook : String -> Book
initBook id =
    { id = id
    , title = "title"
    , price = 0
    , author = "author"
    , content = "content"
    }


createBook : Model -> Model
createBook model =
    let
        book =
            { id = List.length model.books + 1 |> toString
            , title = model.bookForm.title
            , content = model.bookForm.content
            , author = model.bookForm.author
            , price = String.toInt model.bookForm.price |> Result.toMaybe |> Maybe.withDefault 0
            }
    in
    { model | books = book :: model.books }
