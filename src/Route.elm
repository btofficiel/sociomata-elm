module Route exposing (Query, Route(..), Token, matchRoute, parseUrl)

import Url exposing (Url)
import Url.Parser exposing (..)
import Url.Parser.Query as Q


type alias Query =
    { q : Maybe String
    , ts : Maybe Int
    }


type alias Token =
    String


type Route
    = Loading
    | Index
    | CreatePostQuery Query
    | Settings
    | NotFound


query : Q.Parser Query
query =
    Q.map2 Query (Q.string "q") (Q.int "ts")


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
        , map CreatePostQuery (s "app" </> s "create" <?> query)
        , map Loading (s "app" </> s "loading")
        , map Settings (s "app" </> s "settings")
        ]
