module Page.EditDraft exposing (Model, Msg, Tweet, init, update, view)

import Api
import Array
import Browser.Dom as Dom
import Browser.Navigation as Nav
import DateTime exposing (Offset)
import Dict exposing (Dict)
import File exposing (File)
import File.Select as Select
import Html exposing (Attribute, Html, button, div, img, input, li, section, span, text, textarea)
import Html.Attributes exposing (class, id, placeholder, src, style, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Html.Keyed
import Http
import Iso8601
import Json.Decode as D exposing (Decoder, at, field, int, map2, string)
import MessageBanner as Message exposing (MessageBanner)
import Page.CreatePost exposing (FileBatch, FileTuple, buildBatch, fileToTuple, resetTextArea, resizeTextArea, uploadMediaTask)
import Page.Loading
import Post exposing (Post, twitterPostDecoder)
import Profile exposing (Config)
import RemoteData exposing (WebData)
import Request
import Route exposing (Token)
import Task
import Time
import Tuple
import Tweet
    exposing
        ( Media
        , PresignedURL
        , TwitterMediaKey
        , tweetsEncoder
        )


type alias Tweet =
    { tweet : String
    , key : String
    , media : List Media
    }


type ActionType
    = SaveDraft
    | ConvertToSchedule
    | PostNow


type alias Model =
    { tweets : List Tweet
    , message : MessageBanner
    , timestamp : Maybe Int
    , fileBatch : List FileBatch
    , postId : Int
    , post : WebData Post
    , toggleMenu : Bool
    }


type Msg
    = RemoveTweet Int String
    | SendPostNow
    | FadeMessage
    | EnterTweet Int String String
    | EnterTime String
    | PickMedia Int
    | GotMedia Int File (List File)
    | GetMediaKeys ActionType (Result Http.Error (List PresignedURL))
    | GotMediaURL Int (List FileTuple)
    | RemoveMedia Int Int
    | GenerateDefaultTime Time.Posix
    | NewTweet Int
    | AddToThread Int Time.Posix
    | EditDraft Time.Posix
    | TryEditDraft
    | ScheduleDraft Time.Posix
    | TryScheduleDraft
    | GotDraft (WebData Post)
    | GotEditedDraft ActionType (Result Http.Error ())
    | ToggleMenu
    | NoOp


fetchDraft : Token -> Int -> Cmd Msg
fetchDraft token postId =
    let
        url =
            String.concat [ "/api/posts/drafts/", String.fromInt postId ]
    in
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" token ]
        , url = url
        , body = Http.emptyBody
        , expect = Request.expectJson (RemoteData.fromResult >> GotDraft) twitterPostDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


init : Int -> Token -> ( Model, Cmd Msg )
init postId token =
    let
        tweet =
            { tweet = ""
            , key = "tweet#1"
            , media = []
            }
    in
    ( { tweets = [ tweet ]
      , message = Nothing
      , timestamp = Nothing
      , fileBatch = []
      , postId = postId
      , post = RemoteData.Loading
      , toggleMenu = False
      }
    , Cmd.batch [ DateTime.getNewTime GenerateDefaultTime, fetchDraft token postId ]
    )


editDraft : Int -> { tweets : List Tweet, timestamp : Maybe Int, media : List PresignedURL } -> Token -> Cmd Msg
editDraft postId model token =
    let
        decoder =
            D.succeed ()

        url =
            String.concat [ "/api/posts/drafts/", String.fromInt postId ]
    in
    Http.request
        { method = "PUT"
        , headers = [ Http.header "Authorization" token ]
        , url = url
        , body = Http.jsonBody (tweetsEncoder model)
        , expect = Request.expectJson (GotEditedDraft SaveDraft) decoder
        , timeout = Nothing
        , tracker = Nothing
        }


scheduleDraft : Int -> { tweets : List Tweet, timestamp : Maybe Int, media : List PresignedURL } -> Token -> Cmd Msg
scheduleDraft postId model token =
    let
        decoder =
            D.succeed ()

        url =
            String.concat [ "/api/posts/schedule-draft/", String.fromInt postId ]
    in
    Http.request
        { method = "PUT"
        , headers = [ Http.header "Authorization" token ]
        , url = url
        , body = Http.jsonBody (tweetsEncoder model)
        , expect = Request.expectJson (GotEditedDraft ConvertToSchedule) decoder
        , timeout = Nothing
        , tracker = Nothing
        }


postNow : { tweets : List Tweet, timestamp : Maybe Int, media : List PresignedURL } -> Token -> Cmd Msg
postNow model token =
    let
        decoder =
            D.succeed ()
    in
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" token ]
        , url = "/api/posts/now"
        , body = Http.jsonBody (tweetsEncoder model)
        , expect = Request.expectJson (GotEditedDraft PostNow) decoder
        , timeout = Nothing
        , tracker = Nothing
        }


calculateTweetProgress : Int -> String
calculateTweetProgress tweetSize =
    let
        progress =
            toFloat tweetSize
                / 280

        width =
            progress
                |> (*) 8
                |> String.fromFloat
    in
    width


viewTwitterImage : Int -> ( Int, Media ) -> Html Msg
viewTwitterImage tweet_order ( media_order, media ) =
    div [ class "twitter-img-container" ]
        [ img
            [ class "twitter-media"
            , src media.url
            ]
            []
        , img [ class "close", src "/images/closeHover.png", onClick (RemoveMedia tweet_order media_order) ] []
        ]


viewEditor : ( Int, Tweet ) -> Html Msg
viewEditor ( order, tweet ) =
    li []
        [ div []
            [ div [ class "editor" ]
                [ textarea
                    [ placeholder "Write your tweet"
                    , value tweet.tweet
                    , id tweet.key
                    , onInput (EnterTweet order tweet.key)
                    ]
                    []
                , img [ class "close", src "/images/closeHover.png", onClick (RemoveTweet order tweet.key) ] []
                , div [ class "twitter-images" ]
                    (List.indexedMap Tuple.pair tweet.media
                        |> List.map (viewTwitterImage order)
                    )
                , div [ class "post-settings " ]
                    [ span [ class "post-ctas" ]
                        [ img [ src "/images/pickImage.png", onClick (PickMedia order) ] []
                        ]
                    , span [ class "twitter-counter" ]
                        [ span []
                            [ text
                                (String.concat
                                    [ String.fromInt (String.length tweet.tweet)
                                    , "/280"
                                    ]
                                )
                            ]
                        , span [ class "tweet-count-indicator" ]
                            [ span
                                [ class "tweet-count-progress"
                                , style "width"
                                    (String.concat
                                        [ calculateTweetProgress (String.length tweet.tweet)
                                        , "rem"
                                        ]
                                    )
                                ]
                                []
                            ]
                        ]
                    ]
                ]
            , div [ class "add-btn" ]
                [ img [ class "add-icon", src "/images/addHover.png", onClick (NewTweet order) ] []
                ]
            ]
        ]


viewEditors : List ( Int, Tweet ) -> List ( String, Html Msg )
viewEditors tweets =
    List.map (\( order, tweet ) -> ( tweet.key, viewEditor ( order, tweet ) )) tweets


timestampToHumanTime : Maybe Int -> Offset -> String
timestampToHumanTime ts offset =
    case ts of
        Just timestamp ->
            let
                zone =
                    Time.customZone offset []
            in
            DateTime.timestampToHumanTime timestamp zone

        Nothing ->
            ""


viewOptions : Model -> Offset -> Html Msg
viewOptions model offset =
    div []
        [ span [ class "option-name-value" ]
            [ span [ class "w-option-name" ]
                [ text "Schedule Time" ]
            , input
                [ type_ "datetime-local"
                , class "schedule-time"
                , onInput EnterTime
                , value (timestampToHumanTime model.timestamp offset)
                ]
                []
            ]
        , span [ class "buttons" ]
            [ button [ onClick TryEditDraft ] [ text "Save" ]
            , button [ onClick TryScheduleDraft ] [ text "Schedule" ]
            , button [ onClick SendPostNow ] [ text "Post now" ]
            ]
        ]


body : Model -> Offset -> Html Msg
body model offset =
    case model.post of
        RemoteData.NotAsked ->
            Page.Loading.body

        RemoteData.Loading ->
            Page.Loading.body

        RemoteData.Success _ ->
            let
                tweets =
                    List.indexedMap Tuple.pair model.tweets
            in
            div [ class "container" ]
                [ section [ class "creator-two-panels" ]
                    [ div [ class "writer" ]
                        [ section [ class "subheader" ]
                            [ span [] [ text "Edit Draft" ]
                            ]
                        , viewEditors tweets
                            |> Html.Keyed.ul []
                        ]
                    , div [ class "writer-options" ]
                        [ viewOptions model offset
                        ]
                    ]
                ]

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
        [ {--Page.Loading.headerTest route (Profile.getAvatar config) model.toggleMenu ToggleMenu--}
          Message.viewMessage model.message
        , body model (Profile.getOffset config)
        ]


update : Msg -> Model -> Offset -> Token -> ( Model, Cmd Msg )
update msg model offset token =
    case msg of
        GetMediaKeys postType result ->
            case result of
                Ok media ->
                    let
                        payload =
                            { tweets = model.tweets
                            , timestamp = model.timestamp
                            , media = media
                            }

                        cmd =
                            case postType of
                                SaveDraft ->
                                    editDraft model.postId payload token

                                ConvertToSchedule ->
                                    scheduleDraft model.postId payload token

                                PostNow ->
                                    postNow payload token
                    in
                    ( model, cmd )

                Err error ->
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

        PickMedia order ->
            ( model
            , Select.files [ "image/jpeg", "image/jpg", "image/png" ] (GotMedia order)
            )

        GotMedia tweet_order file files ->
            let
                isFileTypeAlllowed =
                    [ "image/jpeg", "image/png", "image/jpg" ]
                        |> List.member (File.mime file)

                new_files =
                    file :: files

                isFileSizeAllowed =
                    File.size file <= 3670016
            in
            case ( isFileTypeAlllowed, isFileSizeAllowed ) of
                ( True, True ) ->
                    if List.length new_files <= 4 then
                        ( model
                        , List.map fileToTuple new_files
                            |> Task.sequence
                            |> Task.perform (GotMediaURL tweet_order)
                        )

                    else
                        ( { model
                            | message = Just (Message.Failure "Sorry! But you can only add upto 4 images in a tweet")
                          }
                        , Message.fadeMessage FadeMessage
                        )

                ( False, _ ) ->
                    ( { model
                        | message = Just (Message.Failure "Please use a JPG or PNG file format for avatar")
                      }
                    , Message.fadeMessage FadeMessage
                    )

                ( _, False ) ->
                    ( { model
                        | message = Just (Message.Failure "Please upload an image upto 3.5 MBs in size")
                      }
                    , Message.fadeMessage FadeMessage
                    )

        GotMediaURL tweet_order files ->
            let
                map =
                    \( order, tw ) ->
                        if order == tweet_order then
                            let
                                media =
                                    tw.media

                                all_media =
                                    media
                                        ++ List.map
                                            (\( file, url ) ->
                                                { newlyAdded = True
                                                , url = url
                                                , imageKey = ""
                                                , file = Just file
                                                }
                                            )
                                            files

                                list_size =
                                    List.length all_media

                                new_media =
                                    if list_size > 4 then
                                        List.drop (list_size - 4) all_media

                                    else
                                        all_media
                            in
                            { tw | media = new_media }

                        else
                            tw

                new_list =
                    List.indexedMap Tuple.pair model.tweets
                        |> List.map map
            in
            ( { model
                | tweets = new_list
              }
            , Cmd.none
            )

        RemoveMedia tweet_order media_order ->
            let
                map =
                    \( order, tw ) ->
                        if order == tweet_order then
                            let
                                filter =
                                    \( morder, media ) -> morder /= media_order

                                new_media =
                                    List.indexedMap Tuple.pair tw.media
                                        |> List.filter filter
                                        |> List.map (\( morder, media ) -> media)
                            in
                            { tw | media = new_media }

                        else
                            tw

                new_list =
                    List.indexedMap Tuple.pair model.tweets
                        |> List.map map
            in
            ( { model | tweets = new_list }
            , Cmd.none
            )

        ToggleMenu ->
            let
                toggle =
                    case model.toggleMenu of
                        True ->
                            False

                        False ->
                            True
            in
            ( { model | toggleMenu = toggle }, Cmd.none )

        GotDraft response ->
            case response of
                RemoteData.Success post ->
                    let
                        tweets =
                            post.tweets
                                |> List.map
                                    (\t ->
                                        { tweet = t.tweet
                                        , key = String.fromInt t.tweetOrder
                                        , media = t.media
                                        }
                                    )
                    in
                    ( { model
                        | timestamp = Just (Post.timestampToInt post.timestamp)
                        , tweets = tweets
                        , post = RemoteData.Success Post.pseudoPost
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | post = response }, Cmd.none )

        GotEditedDraft SaveDraft (Ok _) ->
            ( { model
                | message = Just (Message.Success "Yes! your draft has been updated")
              }
            , Message.fadeMessage FadeMessage
            )

        GotEditedDraft PostNow (Ok _) ->
            ( { model
                | message = Just (Message.Success "Yes! your post has been posted")
              }
            , Message.fadeMessage FadeMessage
            )

        GotEditedDraft ConvertToSchedule (Ok _) ->
            ( { model
                | message = Just (Message.Success "Yes! your post has been scheduled")
              }
            , Message.fadeMessage FadeMessage
            )

        GotEditedDraft _ (Err error) ->
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

        FadeMessage ->
            ( { model | message = Nothing }, Cmd.none )

        SendPostNow ->
            let
                batch =
                    buildBatch model.tweets
            in
            ( { model
                | message = Just Message.Loading
              }
            , List.map (uploadMediaTask token) batch
                |> Task.sequence
                |> Task.attempt (GetMediaKeys PostNow)
            )

        TryEditDraft ->
            let
                batch =
                    buildBatch model.tweets
            in
            ( { model
                | fileBatch = batch
              }
            , DateTime.getNewTime EditDraft
            )

        EditDraft posix ->
            case model.timestamp of
                Just timestamp ->
                    let
                        ts =
                            toFloat (DateTime.posixToSeconds posix)
                                / 60
                                |> floor
                                |> (*) 60

                        diff =
                            timestamp - ts

                        payload =
                            { tweets = model.tweets
                            , timestamp = model.timestamp
                            }
                    in
                    case diff >= 300 of
                        True ->
                            ( { model
                                | message = Just Message.Loading
                              }
                            , List.map (uploadMediaTask token) model.fileBatch
                                |> Task.sequence
                                |> Task.attempt (GetMediaKeys SaveDraft)
                            )

                        False ->
                            ( { model
                                | message = Just (Message.Failure "Please select a datetime atleast 5 mins away from now")
                              }
                            , Message.fadeMessage FadeMessage
                            )

                Nothing ->
                    ( { model
                        | message = Just (Message.Failure "Please select a datetime for the post")
                      }
                    , Message.fadeMessage FadeMessage
                    )

        TryScheduleDraft ->
            let
                batch =
                    buildBatch model.tweets
            in
            ( { model
                | fileBatch = batch
              }
            , DateTime.getNewTime ScheduleDraft
            )

        ScheduleDraft posix ->
            case model.timestamp of
                Just timestamp ->
                    let
                        ts =
                            toFloat (DateTime.posixToSeconds posix)
                                / 60
                                |> floor
                                |> (*) 60

                        diff =
                            timestamp - ts

                        payload =
                            { tweets = model.tweets
                            , timestamp = model.timestamp
                            }
                    in
                    case diff >= 300 of
                        True ->
                            ( { model
                                | message = Just Message.Loading
                              }
                            , List.map (uploadMediaTask token) model.fileBatch
                                |> Task.sequence
                                |> Task.attempt (GetMediaKeys ConvertToSchedule)
                            )

                        False ->
                            ( { model
                                | message = Just (Message.Failure "Please select a datetime atleast 5 mins away from now")
                              }
                            , Message.fadeMessage FadeMessage
                            )

                Nothing ->
                    ( { model
                        | message = Just (Message.Failure "Please select a datetime for the post")
                      }
                    , Message.fadeMessage FadeMessage
                    )

        GenerateDefaultTime posix ->
            let
                ts =
                    toFloat (DateTime.posixToSeconds posix)
                        / 60
                        |> floor
                        |> (*) 60
                        |> (+) 3600
            in
            ( { model | timestamp = Just ts }, Cmd.none )

        EnterTime datetime ->
            let
                posixTime =
                    Iso8601.toTime datetime
            in
            case posixTime of
                Ok posix ->
                    let
                        ts =
                            DateTime.posixToSeconds posix
                                - (offset * 60)
                    in
                    ( { model | timestamp = Just ts }, Cmd.none )

                Err error ->
                    ( model, Cmd.none )

        RemoveTweet tweet_order key ->
            if List.length model.tweets > 1 then
                let
                    filter =
                        \( order, tweet ) -> order /= tweet_order

                    new_list =
                        List.indexedMap Tuple.pair model.tweets
                            |> List.filter filter
                            |> List.map Tuple.second
                in
                ( { model | tweets = new_list }
                , Cmd.none
                )

            else
                let
                    tweet =
                        { tweet = ""
                        , key = key
                        , media = []
                        }
                in
                ( { model | tweets = [ tweet ] }
                , resetTextArea key
                )

        EnterTweet tweet_order key tweet ->
            let
                map =
                    \( order, tw ) ->
                        if order == tweet_order then
                            { tw | tweet = String.slice 0 280 tweet }

                        else
                            tw

                new_list =
                    List.indexedMap Tuple.pair model.tweets
                        |> List.map map
            in
            ( { model
                | tweets = new_list
              }
            , resizeTextArea key
            )

        NewTweet tweet_order ->
            ( model, DateTime.getNewTime (AddToThread tweet_order) )

        AddToThread tweet_order posix ->
            let
                key =
                    Time.posixToMillis posix
                        |> String.fromInt

                first_part =
                    Array.fromList model.tweets
                        |> Array.slice 0 (tweet_order + 1)
                        |> Array.toList

                second_part =
                    Array.fromList model.tweets
                        |> Array.slice (tweet_order + 1) (List.length model.tweets)
                        |> Array.toList

                tweet =
                    { tweet = ""
                    , key = String.concat [ "tweet", key ]
                    , media = []
                    }

                new_list =
                    List.concat [ first_part, [ tweet ], second_part ]

                cmd =
                    Dom.getViewport
                        |> Task.andThen (\info -> Dom.setViewport 0 (info.viewport.y + (0.2 * info.viewport.height)))
                        |> Task.attempt (\_ -> NoOp)
            in
            ( { model
                | tweets = new_list
              }
            , cmd
            )

        NoOp ->
            ( model, Cmd.none )
