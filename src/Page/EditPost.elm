module Page.EditPost exposing (Model, Msg, Tweet, init, update, view)

import Api
import Array
import Browser.Dom as Dom
import Browser.Navigation as Nav
import DateTime exposing (Offset)
import Dict exposing (Dict)
import Html exposing (Attribute, Html, button, div, img, input, li, section, span, text, textarea)
import Html.Attributes exposing (class, id, placeholder, src, style, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Html.Keyed
import Http
import Iso8601
import Json.Decode as D exposing (Decoder, at, field, int, map2, string)
import MessageBanner as Message exposing (MessageBanner)
import Page.CreatePost exposing (resetTextArea, resizeTextArea)
import Page.Loading
import Post exposing (Post, twitterPostDecoder)
import Profile exposing (Config)
import RemoteData exposing (WebData)
import Request
import Route exposing (Token)
import Task
import Time
import Tuple
import Tweet exposing (tweetsEncoder)


type alias Tweet =
    { tweet : String
    , key : String
    }


type alias Model =
    { tweets : List Tweet
    , message : MessageBanner
    , timestamp : Maybe Int
    , postId : Int
    , post : WebData Post
    , toggleMenu : Bool
    }


type Msg
    = CreatePost
    | RemoveTweet Int String
    | FadeMessage
    | EnterTweet Int String String
    | EnterTime String
    | GenerateDefaultTime Time.Posix
    | NewTweet Int
    | AddToThread Int Time.Posix
    | SchedulePost Time.Posix
    | TrySchedulePost
    | GotPost (WebData Post)
    | GotEditedPost (Result Http.Error ())
    | ToggleMenu
    | NoOp


fetchPost : Token -> Int -> Cmd Msg
fetchPost token postId =
    let
        url =
            String.concat [ "/api/posts/", String.fromInt postId ]
    in
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" token ]
        , url = url
        , body = Http.emptyBody
        , expect = Request.expectJson (RemoteData.fromResult >> GotPost) twitterPostDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


init : Int -> Token -> ( Model, Cmd Msg )
init postId token =
    let
        tweet =
            { tweet = ""
            , key = "tweet#1"
            }
    in
    ( { tweets = [ tweet ]
      , message = Nothing
      , timestamp = Nothing
      , postId = postId
      , post = RemoteData.Loading
      , toggleMenu = False
      }
    , Cmd.batch [ DateTime.getNewTime GenerateDefaultTime, fetchPost token postId ]
    )


schedulePost : Int -> { tweets : List Tweet, timestamp : Maybe Int } -> Token -> Cmd Msg
schedulePost postId model token =
    let
        decoder =
            D.succeed ()

        url =
            String.concat [ "/api/posts/", String.fromInt postId ]
    in
    Http.request
        { method = "PUT"
        , headers = [ Http.header "Authorization" token ]
        , url = url
        , body = Http.jsonBody (tweetsEncoder model)
        , expect = Request.expectJson GotEditedPost decoder
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
                , div [ class "post-settings " ]
                    [ span [ class "twitter-counter" ]
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
        , span [ class "button" ]
            [ button [ onClick TrySchedulePost ] [ text "Update post" ]
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
                            [ span [] [ text "Edit Post" ]
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

        GotPost response ->
            case response of
                RemoteData.Success post ->
                    let
                        tweets =
                            post.tweets
                                |> List.map (\t -> { tweet = t.tweet, key = String.fromInt t.tweetOrder })
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

        GotEditedPost (Ok _) ->
            ( { model
                | message = Just (Message.Success "Yes! your post has been updated")
              }
            , Message.fadeMessage FadeMessage
            )

        GotEditedPost (Err error) ->
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

        TrySchedulePost ->
            ( model, DateTime.getNewTime SchedulePost )

        SchedulePost posix ->
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
                            , schedulePost model.postId payload token
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

        CreatePost ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )
