module Page.Queue exposing (Model, Msg, init, update, view)

import Api
import Array
import Browser.Navigation as Nav
import DateTime exposing (Offset, viewTime)
import Dict
import Html exposing (Html, button, div, h3, img, input, li, section, span, text, ul)
import Html.Attributes exposing (class, src, style, type_)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D exposing (Decoder)
import MessageBanner as Message exposing (MessageBanner)
import Page.Loading
import Post exposing (DictPost, Post, PostId, postsQueueDecoder, timestampToInt)
import Profile exposing (Avatar, Config)
import RemoteData exposing (WebData)
import Request
import Route exposing (Token)
import Time exposing (Zone)


type alias Model =
    { posts : WebData DictPost
    , baseTime : Int
    , toggledPost : Maybe PostId
    , message : MessageBanner
    }


type Msg
    = GotPosts (WebData DictPost)
    | OpenCreatePost
    | OpenEditPost PostId
    | FadeMessage
    | GenerateCurrentTime Time.Posix
    | TogglePostActions PostId
    | LoadPosts
    | DeletePost PostId


fetchPosts : Token -> Maybe Int -> Cmd Msg
fetchPosts token ts =
    let
        url =
            case ts of
                Just timestamp ->
                    String.concat [ "/api/posts/queue?timestamp=", String.fromInt timestamp ]

                Nothing ->
                    "/api/posts/queue"
    in
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" token ]
        , url = url
        , body = Http.emptyBody
        , expect = Request.expectJson (RemoteData.fromResult >> GotPosts) postsQueueDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


init : Token -> ( Model, Cmd Msg )
init token =
    ( { posts = RemoteData.Loading
      , baseTime = 0
      , toggledPost = Nothing
      , message = Nothing
      }
    , Cmd.batch [ fetchPosts token Nothing, DateTime.getNewTime GenerateCurrentTime ]
    )


newListRange : Int -> List Int
newListRange range =
    if range > 0 then
        List.range 1 range

    else
        []


viewNameOfDay : String -> Int -> Zone -> String
viewNameOfDay timestamp todayTimestamp zone =
    let
        tsInt =
            String.toInt timestamp

        tommorrowTimestamp =
            todayTimestamp + 86400
    in
    case tsInt of
        Just ts ->
            if ts == todayTimestamp then
                "Today"

            else if ts == tommorrowTimestamp then
                "Tomorrow"

            else
                let
                    tsInMillis =
                        ts * 1000

                    posix =
                        Time.millisToPosix tsInMillis

                    currentYear =
                        String.fromInt (Time.toYear zone posix)
                in
                DateTime.posixToDate posix currentYear zone

        Nothing ->
            "Someday"


viewPost : Maybe PostId -> Zone -> Avatar -> Post -> Html Msg
viewPost toggledPost timezone avatar post =
    let
        timestamp =
            timestampToInt post.timestamp
    in
    li [ class "inqueue" ]
        [ img [ class "avatar", src (Page.Loading.getAvatarURL avatar) ] []
        , img [ class "site", src "/images/twitter.png" ] []
        , span [ class "content", onClick (OpenEditPost post.id) ] [ text post.description ]
        , span [ class "time" ] [ text (viewTime timestamp timezone) ]
        , img
            [ class "settings-icon"
            , src "/images/settings.svg"
            , onClick (TogglePostActions post.id)
            ]
            []
        , span
            [ class "post-menu"
            , style "display"
                (case toggledPost of
                    Just postId ->
                        case Post.compareById postId post.id of
                            True ->
                                "block"

                            False ->
                                "none"

                    Nothing ->
                        "none"
                )
            ]
            [ ul [ class "settings-menu" ]
                [ li [ onClick (OpenEditPost post.id) ] [ text "Edit post" ]
                , li [ onClick (DeletePost post.id) ] [ text "Delete post" ]
                ]
            ]
        ]


viewAddToQueue : Int -> Html Msg
viewAddToQueue _ =
    li [ class "add-to-queue", onClick OpenCreatePost ]
        [ img [ src "/images/add.svg" ] []
        , span [] [ text "Add a post to queue" ]
        ]


viewPostsForADay : Maybe PostId -> List Post -> Zone -> Avatar -> Html Msg
viewPostsForADay toggledPost posts timezone avatar =
    if List.length posts >= 3 then
        ul []
            (List.map (viewPost toggledPost timezone avatar) posts)

    else
        ul []
            (List.append
                (List.map (viewPost toggledPost timezone avatar) posts)
                (List.map viewAddToQueue (newListRange (3 - List.length posts)))
            )


viewDay : Maybe PostId -> Int -> Zone -> Avatar -> ( String, List Post ) -> Html Msg
viewDay toggledPost baseTime timezone avatar ( timestamp, posts ) =
    div [ class "day" ]
        [ h3 [] [ text (viewNameOfDay timestamp baseTime timezone) ]
        , viewPostsForADay toggledPost posts timezone avatar
        ]


body : Model -> WebData Config -> Html Msg
body model config =
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
                        , span [ onClick OpenCreatePost ] [ text "Create Post" ]
                        ]
                    , section [ class "one-panel" ]
                        [ div [ class "queue" ]
                            (List.map (viewDay model.toggledPost model.baseTime timezone avatar) posts
                                ++ [ span [ class "button-container" ]
                                        [ button [ class "load-more", onClick LoadPosts ]
                                            [ text "Show more posts" ]
                                        ]
                                   ]
                            )
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


view : Model -> WebData Config -> Html Msg
view model config =
    div []
        [ Message.viewMessage model.message
        , body model config

        {---
        , div [] [ text (String.concat [ DateTime.posixToDate (DateTime.secondsToPosix 1426444200), " ", Debug.toString model.posts ]) ]
        ---}
        ]


update : Msg -> Model -> Offset -> Token -> Nav.Key -> ( Model, Cmd Msg )
update msg model offset token navKey =
    case msg of
        OpenEditPost (Post.PostId postId) ->
            ( model, Nav.pushUrl navKey (String.concat [ "/app/edit/", String.fromInt postId ]) )

        OpenCreatePost ->
            ( model, Nav.pushUrl navKey "/app/create" )

        FadeMessage ->
            ( { model | message = Nothing }, Cmd.none )

        TogglePostActions postId ->
            case model.toggledPost of
                Just pid ->
                    case Post.compareById pid postId of
                        True ->
                            ( { model | toggledPost = Nothing }, Cmd.none )

                        False ->
                            ( { model | toggledPost = Just postId }, Cmd.none )

                Nothing ->
                    ( { model | toggledPost = Just postId }, Cmd.none )

        LoadPosts ->
            case model.posts of
                RemoteData.Success posts ->
                    let
                        ts =
                            case Dict.isEmpty posts of
                                True ->
                                    Nothing

                                False ->
                                    Dict.keys posts
                                        |> Array.fromList
                                        |> Array.get (Dict.size posts - 1)

                        tsToInt =
                            case ts of
                                Just legitTs ->
                                    String.toInt legitTs

                                Nothing ->
                                    Nothing
                    in
                    ( { model | message = Just Message.Loading }, fetchPosts token tsToInt )

                _ ->
                    ( { model | message = Just (Message.Failure "Sorry, something happened from our end. Try reloading the app") }, Cmd.none )

        GotPosts response ->
            case model.posts of
                RemoteData.Success posts ->
                    case response of
                        RemoteData.Success newPosts ->
                            let
                                updatedPosts =
                                    Dict.union posts newPosts
                            in
                            ( { model
                                | posts = RemoteData.Success updatedPosts
                                , message = Nothing
                              }
                            , Cmd.none
                            )

                        RemoteData.Failure (Http.BadBody err) ->
                            ( { model | message = Just (Message.Failure err) }, Message.fadeMessage FadeMessage )

                        RemoteData.Failure Http.NetworkError ->
                            let
                                err =
                                    "Oops! Looks like there is some problem with your network."
                            in
                            ( { model | message = Just (Message.Failure err) }, Message.fadeMessage FadeMessage )

                        RemoteData.Failure (Http.BadStatus 401) ->
                            ( model
                            , Cmd.batch
                                [ Api.deleteToken ()
                                , Nav.load "/login"
                                ]
                            )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    case response of
                        RemoteData.Failure (Http.BadStatus 401) ->
                            ( model
                            , Cmd.batch
                                [ Api.deleteToken ()
                                , Nav.load "/login"
                                ]
                            )

                        _ ->
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
