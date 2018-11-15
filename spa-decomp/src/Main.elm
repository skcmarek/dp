module Main exposing (initModel, main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models.Models as Models
import Navigation exposing (Location)
import Routes exposing (..)
import Update
import UrlParser exposing (..)
import View.Elements as Elements
import View.Layouts as Layouts


main : Program Never Models.Model Update.Msg
main =
    Navigation.program Update.OnLocationChange
        { init = \location -> ( { initModel | route = Routes.getLoc location }, Cmd.none )
        , view = Layouts.view
        , update = Update.update
        , subscriptions = \model -> Sub.none
        }


initModel : Models.Model
initModel =
    { books = List.range 1 10 |> List.map Basics.toString |> List.map Models.initBook
    , user = Just { email = "email@gmail.com" }
    , route = IndexRoute
    , loginForm = { email = "", password = "" }
    , signupForm = { email = "", password = "", passwordAgain = "" }
    , bookForm = { title = "", author = "", price = "", content = "" }
    }
