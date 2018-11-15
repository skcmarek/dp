module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model


main : Program Never Model.Model Model.Msg
main =
    Html.program
        { init = Model.init
        , update = Model.update
        , subscriptions = \model -> Sub.none
        , view = view
        }


view : Model.Model -> Html Model.Msg
view model =
    div []
        []
