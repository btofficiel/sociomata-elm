port module Page.CreatePost exposing (Model, Msg, Tweet, init, resetTextArea, resizeTextArea, update, view)

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
import Page.Loading
import Profile exposing (Config)
import RemoteData exposing (WebData)
import Request
import Route exposing (Query, Token)
import Task
import Time
import Tuple
import Tweet exposing (Media, tweetsEncoder)


type alias Tweet =
    { tweet : String
    , key : String
    , media : List Media
    }


type alias Model =
    { tweets : List Tweet
    , message : MessageBanner
    , timestamp : Maybe Int
    }


type ActionType
    = Schedule
    | PostNow


type Msg
    = SendPostNow
    | RemoveTweet Int String
    | FadeMessage
    | EnterTweet Int String String
    | EnterTime String
    | PickMedia Int
    | GotMedia Int File (List File)
    | GotMediaURL Int (List String)
    | RemoveMedia Int Int
    | GenerateDefaultTime Time.Posix
    | NewTweet Int
    | AddToThread Int Time.Posix
    | SchedulePost Time.Posix
    | TrySchedulePost
    | GotCreatedPost ActionType (Result Http.Error ())
    | NoOp


init : Query -> ( Model, Cmd Msg )
init query =
    let
        tweet =
            { tweet = ""
            , key = "tweet#1"
            , media = []
            }
    in
    ( { tweets = [ tweet ]
      , message = Nothing
      , timestamp = query.timestamp
      }
    , DateTime.getNewTime GenerateDefaultTime
    )


schedulePost : { tweets : List Tweet, timestamp : Maybe Int } -> Token -> Cmd Msg
schedulePost model token =
    let
        decoder =
            D.succeed ()
    in
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" token ]
        , url = "/api/posts"
        , body = Http.jsonBody (tweetsEncoder model)
        , expect = Request.expectJson (GotCreatedPost Schedule) decoder
        , timeout = Nothing
        , tracker = Nothing
        }


postNow : { tweets : List Tweet, timestamp : Maybe Int } -> Token -> Cmd Msg
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
        , expect = Request.expectJson (GotCreatedPost PostNow) decoder
        , timeout = Nothing
        , tracker = Nothing
        }


port resizeTextArea : String -> Cmd msg


port resetTextArea : String -> Cmd msg


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
            [ button [ onClick TrySchedulePost ] [ text "Schedule" ]
            , button [ onClick SendPostNow ] [ text "Post now" ]
            ]
        ]


body : Model -> Offset -> Html Msg
body model offset =
    let
        tweets =
            List.indexedMap Tuple.pair model.tweets
    in
    div [ class "container" ]
        [ section [ class "creator-two-panels" ]
            [ div [ class "writer" ]
                [ section [ class "subheader" ]
                    [ span [] [ text "Create Post" ]
                    ]
                , viewEditors tweets
                    |> Html.Keyed.ul []
                ]
            , div [ class "writer-options" ]
                [ viewOptions model offset
                ]
            ]
        ]


view : Model -> WebData Config -> Html Msg
view model config =
    div []
        [ Message.viewMessage model.message
        , body model (Profile.getOffset config)
        ]


update : Msg -> Model -> Offset -> Token -> ( Model, Cmd Msg )
update msg model offset token =
    case msg of
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
            in
            case isFileTypeAlllowed of
                True ->
                    if List.length new_files <= 4 then
                        ( model
                        , List.map File.toUrl new_files
                            |> Task.sequence
                            |> Task.perform (GotMediaURL tweet_order)
                        )

                    else
                        ( { model
                            | message = Just (Message.Failure "Sorry! But you can only add upto 4 images in a tweet")
                          }
                        , Message.fadeMessage FadeMessage
                        )

                False ->
                    ( { model
                        | message = Just (Message.Failure "Please use a JPG or PNG file format for avatar")
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
                                    media ++ List.map (\f -> { newlyAdded = True, url = f }) files

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

        GotCreatedPost Schedule (Ok _) ->
            ( { model
                | message = Just (Message.Success "Yes! your post is scheduled")
              }
            , Message.fadeMessage FadeMessage
            )

        GotCreatedPost PostNow (Ok _) ->
            ( { model
                | message = Just (Message.Success "Yes! your tweets have been posted")
              }
            , Message.fadeMessage FadeMessage
            )

        GotCreatedPost _ (Err error) ->
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
                payload =
                    { tweets = model.tweets
                    , timestamp = model.timestamp
                    }
            in
            ( { model
                | message = Just Message.Loading
              }
            , postNow payload token
            )

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
                            , schedulePost payload token
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
            case model.timestamp of
                Just _ ->
                    ( model, Cmd.none )

                Nothing ->
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
