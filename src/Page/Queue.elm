module Page.Queue exposing (Model, Msg, init, update, view)

import DateTime exposing (Offset, viewTime)
import Dict
import Html exposing (Html, div, h3, img, input, li, section, span, text, ul)
import Html.Attributes exposing (class, src, type_)
import Html.Events exposing (onClick, onInput)
import Http
import Iso8601
import Page.Loading exposing (header)
import Post exposing (DictPost, Post, postsQueueDecoder, timestampToInt)
import Profile exposing (Avatar, Config)
import RemoteData exposing (WebData)
import Request
import Route exposing (Route, Token)
import Time exposing (Zone)


type alias Model =
    { posts : WebData DictPost
    , baseTime : Int
    }


type Msg
    = GotPosts (WebData DictPost)
    | GenerateCurrentTime Time.Posix
    | StoreDateTime String


fetchPosts : Token -> Cmd Msg
fetchPosts token =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" token ]
        , url = "/api/posts"
        , body = Http.emptyBody
        , expect = Request.expectJson (RemoteData.fromResult >> GotPosts) postsQueueDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


init : Token -> ( Model, Cmd Msg )
init token =
    ( { posts = RemoteData.Loading
      , baseTime = 0
      }
    , Cmd.batch [ fetchPosts token, DateTime.getNewTime GenerateCurrentTime ]
    )


newListRange : Int -> List Int
newListRange range =
    if range > 0 then
        List.range 1 range

    else
        []


viewPost : Zone -> Avatar -> Post -> Html Msg
viewPost timezone avatar post =
    let
        timestamp =
            timestampToInt post.timestamp
    in
    li [ class "inqueue" ]
        [ img [ class "avatar", src (Page.Loading.getAvatarURL avatar) ] []
        , img [ class "site", src "/images/twitter.png" ] []
        , span [ class "content" ] [ text post.description ]
        , span [ class "time" ] [ text (viewTime timestamp timezone) ]
        , img [ src "/images/settings.svg" ] []
        ]


viewAddToQueue : Int -> Html Msg
viewAddToQueue _ =
    li [ class "add-to-queue" ]
        [ img [ src "/images/add.svg" ] []
        , span [] [ text "Add a post to queue" ]
        ]


viewPostsForADay : List Post -> Zone -> Avatar -> Html Msg
viewPostsForADay posts timezone avatar =
    if List.length posts >= 3 then
        ul []
            (List.map (viewPost timezone avatar) posts)

    else
        ul []
            (List.append
                (List.map (viewPost timezone avatar) posts)
                (List.map viewAddToQueue (newListRange (3 - List.length posts)))
            )


viewDay : Int -> Zone -> Avatar -> ( String, List Post ) -> Html Msg
viewDay baseTime timezone avatar ( timestamp, posts ) =
    div [ class "day" ]
        [ h3 [] [ text timestamp ]
        , viewPostsForADay posts timezone avatar
        ]


body : Model -> Token -> WebData Config -> Html Msg
body model token config =
    let
        timezone =
            Time.customZone (Profile.getOffset config) []

        avatar =
            Profile.getAvatar config
    in
    case model.posts of
        RemoteData.NotAsked ->
            Page.Loading.body

        RemoteData.Loading ->
            Page.Loading.body

        RemoteData.Success postsDict ->
            let
                posts =
                    Dict.toList postsDict
            in
            div [ class "container" ]
                [ section [ class "panels" ]
                    [ section [ class "subheader" ]
                        [ span [] [ text "Queue" ]
                        , span [] [ text "Create Post" ]
                        ]
                    , section [ class "one-panel" ]
                        [ div [ class "queue" ]
                            (List.map (viewDay model.baseTime timezone avatar) posts)
                        ]
                    ]
                ]

        {--

                    div [ class "container" ]
                        [ section [ class "panels" ]
                            [ section [ class "subheader" ]
                                [ span [] [ text "Queue" ]
                                , span [] [ text "Create Post" ]
                                ]
                            , Page.Loading.emptyState 60 "It's empty over here. Go ahead and create a post"
                            , input [ type_ "datetime-local", onInput StoreDateTime ] []
                            ]
                        ]
        --}
        RemoteData.Failure (Http.BadBody err) ->
            div [ class "container" ]
                [ section [ class "panels" ]
                    [ Page.Loading.emptyState 80 err
                    ]
                ]

        RemoteData.Failure Http.NetworkError ->
            div [ class "container" ]
                [ section [ class "panels" ]
                    [ Page.Loading.emptyState 80 "Oops! Looks like there is some problem with your network."
                    ]
                ]

        _ ->
            div [] []


view : Model -> Token -> Route -> WebData Config -> Html Msg
view model token route config =
    div []
        [ header route (Profile.getAvatar config)
        , body model token config

        {---
        , div [] [ text (String.concat [ DateTime.posixToDate (DateTime.secondsToPosix 1426444200), " ", Debug.toString model.posts ]) ]
        ---}
        ]


update : Msg -> Model -> Offset -> ( Model, Cmd Msg )
update msg model offset =
    case msg of
        GotPosts response ->
            ( { model | posts = response }, Cmd.none )

        GenerateCurrentTime currentTime ->
            let
                ts =
                    (toFloat (DateTime.posixToSeconds currentTime)
                        / 86400
                        |> floor
                        |> (*) 86400
                    )
                        - (offset * 60)
            in
            ( { model | baseTime = ts }, Cmd.none )

        StoreDateTime str ->
            case Iso8601.toTime str of
                Ok posix ->
                    let
                        ts =
                            String.fromInt (Time.posixToMillis posix)
                    in
                    Debug.log ts
                        ( model, Cmd.none )

                Err error ->
                    ( model, Cmd.none )
