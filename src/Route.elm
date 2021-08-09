module Route exposing (Query, Route(..), Token, matchRoute, parseUrl)

import Url exposing (Url)
import Url.Parser exposing (..)
import Url.Parser.Query as Q


type alias Query =
    { timestamp : Maybe Int
    }


type alias Token =
    String


type Route
    = Loading
    | Index
    | CreatePost Query
    | EditPost Int
    | Settings
    | NotFound


query : Q.Parser Query
query =
    Q.map Query (Q.int "timestamp")


parseUrl : Url -> Route
parseUrl url =
    case parse matchRoute url of
        Just route ->
            route

        Nothing ->
            NotFound


matchRoute : Parser (Route -> a) a
matchRoute =
    oneOf
        [ map Index (s "app")
        , map CreatePost (s "app" </> s "create" <?> query)
        , map EditPost (s "app" </> s "edit" </> int)
        , map Loading (s "app" </> s "loading")
        , map Settings (s "app" </> s "settings")
        ]
