module Update exposing (Msg(..), reroute, update)

import Models.Models as Models
import Navigation exposing (Location)
import Routes exposing (..)


type Msg
    = OnLocationChange Location
    | UpdateRoute Route
    | OnInputLogin Models.SignupForm
    | OnInputBook Models.BookForm
    | OnInputSignup Models.SignupForm
    | AddBook
    | Login
    | Logout
    | SignUp


update : Msg -> Models.Model -> ( Models.Model, Cmd Msg )
update msg model =
    case msg of
        OnInputBook form ->
            ( { model | bookForm = form }, Cmd.none )

        OnInputSignup form ->
            ( { model | signupForm = form }, Cmd.none )

        OnInputLogin form ->
            ( { model | signupForm = form }, Cmd.none )

        OnLocationChange location ->
            { model | route = Routes.getLoc location } |> reroute

        UpdateRoute route ->
            ( model, Navigation.newUrl <| Routes.path route )

        SignUp ->
            ( model, Navigation.newUrl <| Routes.path Routes.LoginRoute )

        Login ->
            { model | user = Just { email = model.signupForm.email } } |> reroute

        Logout ->
            ( { model | user = Nothing }, Navigation.newUrl <| Routes.path Routes.IndexRoute )

        AddBook ->
            ( Models.createBook model, Navigation.newUrl <| Routes.path Routes.IndexRoute )


reroute : Models.Model -> ( Models.Model, Cmd msg )
reroute model =
    case ( model.route, model.user ) of
        ( Routes.LoginRoute, Just _ ) ->
            ( model, Navigation.modifyUrl <| Routes.path Routes.IndexRoute )

        ( Routes.SignUpRoute, Just _ ) ->
            ( model, Navigation.modifyUrl <| Routes.path Routes.IndexRoute )

        ( Routes.AddBookRoute, Nothing ) ->
            ( { model | route = ErrRoute }, Cmd.none )

        _ ->
            ( model, Cmd.none )
