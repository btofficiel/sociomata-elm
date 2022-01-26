port module Page.Queue exposing (Model, Msg, ToggledPost, init, subscriptions, update, view)

import Api
import Array
import Browser.Navigation as Nav
import DateTime exposing (Offset, viewTime)
import Dict
import Html exposing (Html, button, div, h3, img, input, li, section, span, text, ul)
import Html.Attributes exposing (class, id, src, style, type_)
import Html.Events exposing (onClick, onInput)
import Html.Keyed
import Http
import Json.Decode as D
import MessageBanner as Message exposing (MessageBanner)
import Page.Loading
import Post exposing (DictPost, Post, PostId, postsQueueDecoder, timestampToInt)
import Profile exposing (Avatar, Config, displayOnboardingMessage)
import RemoteData exposing (WebData)
import Request
import Route exposing (Token)
import Time exposing (Zone)


type alias Model =
    { posts : WebData DictPost
    , baseTime : Int
    , toggledPost : ToggledPost
    , message : MessageBanner
    }


type alias ToggledPost =
    { post : Maybe PostId
    , askDeleteConfirmation : Bool
    }


type Msg
    = GotPosts (WebData DictPost)
    | OpenCreatePost (Maybe Int)
    | OpenEditPost PostId
    | FadeMessage
    | GenerateCurrentTime Time.Posix
    | TogglePostActions PostId
    | LoadPosts
    | DeletePost String PostId
    | GotDeletedPost ( String, Int ) (Result Http.Error ())
    | PromptDeletePost
    | TurnOffPostMenu


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


deletePost : Token -> String -> Int -> Cmd Msg
deletePost token day postId =
    let
        decoder =
            D.succeed ()

        url =
            String.concat [ "/api/posts/", String.fromInt postId ]
    in
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "Authorization" token ]
        , url = url
        , body = Http.emptyBody
        , expect = Request.expectJson (GotDeletedPost ( day, postId )) decoder
        , timeout = Nothing
        , tracker = Nothing
        }


init : Token -> WebData Config -> ( Model, Cmd Msg )
init token config =
    let
        message =
            displayOnboardingMessage config
    in
    ( { posts = RemoteData.Loading
      , baseTime = 0
      , toggledPost =
            { post = Nothing
            , askDeleteConfirmation = False
            }
      , message = message
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


viewPost : ToggledPost -> Zone -> Avatar -> String -> Post -> Html Msg
viewPost toggledPost timezone avatar day post =
    let
        timestamp =
            timestampToInt post.timestamp

        postIdString =
            Post.postIdToInt post.id
                |> String.fromInt

        settingsIconId =
            postIdString
                |> (++) "settingsIcon"

        postMenuId =
            postIdString
                |> (++) "postMenu"
    in
    li [ class "inqueue" ]
        [ img [ class "avatar", src (Page.Loading.getAvatarURL avatar) ] []
        , img [ class "site", src "/images/twitter.png" ] []
        , span [ class "content", onClick (OpenEditPost post.id) ] [ text post.description ]
        , span [ class "time" ] [ text (viewTime timestamp timezone) ]
        , img
            [ id settingsIconId
            , class "settings-icon"
            , src "/images/settings.svg"
            , onClick (TogglePostActions post.id)
            ]
            []
        , span
            [ id postMenuId
            , class "post-menu"
            , style "display"
                (case toggledPost.post of
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
            (case toggledPost.askDeleteConfirmation of
                True ->
                    [ div [] [ text "Delete this post?" ]
                    , ul [ class "settings-menu" ]
                        [ li [ onClick (DeletePost day post.id) ] [ text "Yes" ]
                        , li [ onClick TurnOffPostMenu ] [ text "No" ]
                        ]
                    ]

                False ->
                    [ ul [ class "settings-menu" ]
                        [ li [ onClick (OpenEditPost post.id) ] [ text "Edit post" ]
                        , li [ onClick PromptDeletePost ] [ text "Delete post" ]
                        ]
                    ]
            )
        ]


viewAddToQueue : String -> Int -> Html Msg
viewAddToQueue day _ =
    li [ class "add-to-queue", onClick (OpenCreatePost (String.toInt day)) ]
        [ img [ src "/images/add.svg" ] []
        , span [] [ text "Add a post to queue" ]
        ]


viewPostsForADay : ToggledPost -> List Post -> Zone -> Avatar -> String -> Html Msg
viewPostsForADay toggledPost posts timezone avatar day =
    if List.length posts >= 3 then
        List.map
            (\post ->
                ( Post.postIdToInt post.id
                    |> String.fromInt
                , viewPost toggledPost timezone avatar day post
                )
            )
            posts
            |> Html.Keyed.ul []

    else
        (List.map
            (\post ->
                ( Post.postIdToInt post.id
                    |> String.fromInt
                , viewPost toggledPost timezone avatar day post
                )
            )
            posts
            ++ List.map
                (\order ->
                    ( day
                        |> (++) (String.fromInt order)
                    , viewAddToQueue day order
                    )
                )
                (newListRange (3 - List.length posts))
        )
            |> Html.Keyed.ul []


viewDay : ToggledPost -> Int -> Zone -> Avatar -> ( String, List Post ) -> Html Msg
viewDay toggledPost baseTime timezone avatar ( timestamp, posts ) =
    div [ class "day" ]
        [ h3 [] [ text (viewNameOfDay timestamp baseTime timezone) ]
        , viewPostsForADay toggledPost posts timezone avatar timestamp
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
                        , span [ onClick (OpenCreatePost Nothing) ] [ text "Create Post" ]
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


port setToggledPostIds : Int -> Cmd msg


port turnOffPostMenu : (() -> msg) -> Sub msg


subscriptions : Sub Msg
subscriptions =
    turnOffPostMenu (always TurnOffPostMenu)


update : Msg -> Model -> Offset -> Token -> Nav.Key -> ( Model, Cmd Msg )
update msg model offset token navKey =
    case msg of
        PromptDeletePost ->
            let
                currentToggledPost =
                    model.toggledPost

                new_togglePost =
                    { currentToggledPost
                        | askDeleteConfirmation = True
                    }
            in
            ( { model
                | toggledPost = new_togglePost
              }
            , Cmd.none
            )

        TurnOffPostMenu ->
            ( { model
                | toggledPost =
                    { post = Nothing
                    , askDeleteConfirmation = False
                    }
              }
            , Cmd.none
            )

        OpenEditPost (Post.PostId postId) ->
            ( model, Nav.pushUrl navKey (String.concat [ "/app/edit/", String.fromInt postId ]) )

        OpenCreatePost Nothing ->
            ( model, Nav.pushUrl navKey "/app/create" )

        OpenCreatePost (Just day) ->
            case day == model.baseTime of
                True ->
                    let
                        url =
                            day
                                |> (+) (86400 - 60)
                                |> String.fromInt
                                |> (++) "/app/create?timestamp="
                    in
                    ( model, Nav.pushUrl navKey url )

                False ->
                    let
                        url =
                            String.fromInt day
                                |> (++) "/app/create?timestamp="
                    in
                    ( model, Nav.pushUrl navKey url )

        FadeMessage ->
            ( { model | message = Nothing }, Cmd.none )

        TogglePostActions postId ->
            case model.toggledPost.post of
                Just pid ->
                    case Post.compareById pid postId of
                        True ->
                            ( { model
                                | toggledPost =
                                    { post = Nothing
                                    , askDeleteConfirmation = False
                                    }
                              }
                            , Cmd.none
                            )

                        False ->
                            ( { model
                                | toggledPost =
                                    { post = Just postId
                                    , askDeleteConfirmation = False
                                    }
                              }
                            , setToggledPostIds (Post.postIdToInt postId)
                            )

                Nothing ->
                    ( { model
                        | toggledPost =
                            { post = Just postId
                            , askDeleteConfirmation = False
                            }
                      }
                    , setToggledPostIds (Post.postIdToInt postId)
                    )

        DeletePost day (Post.PostId postId) ->
            ( { model
                | message = Just Message.Loading
              }
            , deletePost token day postId
            )

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

        GotDeletedPost ( day, postId ) (Ok _) ->
            case model.posts of
                RemoteData.Success postDict ->
                    case Dict.get day postDict of
                        Just posts ->
                            let
                                new_posts =
                                    posts
                                        |> List.filter (\p -> Post.compareById p.id (Post.PostId postId) == False)

                                updater maybePost =
                                    Just new_posts

                                newDict =
                                    Dict.update day updater postDict
                            in
                            ( { model
                                | message = Just (Message.Success "Your post has been deleted")
                                , posts = RemoteData.Success newDict
                              }
                            , Message.fadeMessage FadeMessage
                            )

                        Nothing ->
                            ( model, Nav.reload )

                _ ->
                    ( model, Nav.reload )

        GotDeletedPost _ (Err error) ->
            case error of
                Http.BadBody err ->
                    ( { model | message = Just (Message.Failure err) }, Message.fadeMessage FadeMessage )

                Http.NetworkError ->
                    let
                        err =
                            "Oops! Looks like there is some problem with your network."
                    in
                    ( { model | message = Just (Message.Failure err) }, Message.fadeMessage FadeMessage )

                Http.BadStatus 401 ->
                    ( model
                    , Cmd.batch
                        [ Api.deleteToken ()
                        , Nav.load "/login"
                        ]
                    )

                _ ->
                    let
                        err =
                            "Oops! Something bad happened, please try reloading the app"
                    in
                    ( { model | message = Just (Message.Failure err) }, Message.fadeMessage FadeMessage )

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
                currentTs =
                    DateTime.posixToSeconds currentTime

                ts =
                    (toFloat currentTs
                        / 86400
                        |> floor
                        |> (*) 86400
                    )
                        - (offset * 60)

                adjustedTs =
                    case offset < 0 of
                        True ->
                            ts - 86400

                        False ->
                            ts + 86400

                finalTs =
                    case currentTs - ts >= 86400 of
                        True ->
                            adjustedTs

                        False ->
                            ts
            in
            ( { model | baseTime = finalTs }, Cmd.none )
