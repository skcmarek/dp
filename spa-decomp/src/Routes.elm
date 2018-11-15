module Routes exposing (Route(..), getLoc, path, routingMatch)

import Navigation exposing (Location)
import UrlParser exposing (..)


type Route
    = IndexRoute
    | BookDetailRoute String
    | AddBookRoute
    | LoginRoute
    | SignUpRoute
    | ErrRoute


getLoc : Location -> Route
getLoc location =
    case parseHash routingMatch location of
        Just route ->
            route

        Nothing ->
            Debug.log (Basics.toString location)
                ErrRoute


routingMatch : Parser (Route -> a) a
routingMatch =
    oneOf
        [ UrlParser.map IndexRoute top
        , UrlParser.map IndexRoute (UrlParser.s "books")
        , UrlParser.map BookDetailRoute (UrlParser.s "books" </> UrlParser.string)
        , UrlParser.map AddBookRoute (UrlParser.s "addBook")
        , UrlParser.map LoginRoute (UrlParser.s "login")
        , UrlParser.map SignUpRoute (UrlParser.s "signup")
        ]


path : Route -> String
path route =
    case route of
        IndexRoute ->
            "#books"

        BookDetailRoute id ->
            "#books/" ++ id

        AddBookRoute ->
            "#addBook"

        LoginRoute ->
            "#login"

        SignUpRoute ->
            "#signup"

        ErrRoute ->
            ""
