module Tweet exposing
    ( Media
    , PresignedURL
    , Tweet
    , TwitterMediaKey
    , encodeNullable
    , mediaBatchPresignedEncoder
    , mediaForPresignedEncoder
    , presignedURLDecoder
    , tweetsDecoder
    , tweetsEncoder
    )

import Api
import File exposing (File)
import Json.Decode as Decode exposing (Decoder, bool, int, list, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import Media
import Tuple


type alias TwitterMediaKey =
    { newlyAdded : Bool
    , key : String
    }


type alias Tweet =
    { tweet : String
    , tweetOrder : Int
    , media : List Media
    }


type alias Media =
    { newlyAdded : Bool
    , url : String
    , imageKey : String
    , file : Maybe File
    }


encodeNullable : (value -> Encode.Value) -> Maybe value -> Encode.Value
encodeNullable valueEncoder maybeValue =
    case maybeValue of
        Just value ->
            valueEncoder value

        Nothing ->
            Encode.null


type alias PresignedURL =
    { newlyAdded : Bool
    , tweetOrder : Int
    , mediaOrder : Int
    , key : String
    , mime : String
    , url : String
    , fields : Media.Media
    }


presignedURLDecoder : Decoder PresignedURL
presignedURLDecoder =
    Decode.succeed PresignedURL
        |> required "newly_added" bool
        |> required "tweet_order" int
        |> required "media_order" int
        |> required "key" string
        |> required "mime" string
        |> required "url" string
        |> optional "fields" Media.mediaDecoder Media.emptyMedia


presignedURLsDecoder : Decoder (List PresignedURL)
presignedURLsDecoder =
    Decode.at [ "data", "mediaBatch" ] (list presignedURLDecoder)


mediaDecoder : Decoder Media
mediaDecoder =
    Decode.succeed Media
        |> required "newly_added" bool
        |> required "url" string
        |> required "media_key" string
        |> hardcoded Nothing


mediaeDecoder : Decoder (List Media)
mediaeDecoder =
    list mediaDecoder


tweetDecoder : Decoder Tweet
tweetDecoder =
    Decode.succeed Tweet
        |> required "tweet" string
        |> required "tweet_order" int
        |> required "media" mediaeDecoder


tweetsDecoder : Decoder (List Tweet)
tweetsDecoder =
    list tweetDecoder


mediaEncoder : PresignedURL -> Encode.Value
mediaEncoder media =
    Encode.object
        [ ( "newly_added", Encode.bool media.newlyAdded )
        , ( "tweet_order", Encode.int media.tweetOrder )
        , ( "media_order", Encode.int media.mediaOrder )
        , ( "key"
          , Encode.string
                (case media.newlyAdded of
                    True ->
                        media.fields.key

                    False ->
                        media.key
                )
          )
        ]


mediaForPresignedEncoder :
    { newlyAdded : Bool
    , tweetOrder : Int
    , mediaOrder : Int
    , key : String
    , file : Maybe File
    , mime : String
    }
    -> Encode.Value
mediaForPresignedEncoder media =
    Encode.object
        [ ( "newly_added", Encode.bool media.newlyAdded )
        , ( "tweet_order", Encode.int media.tweetOrder )
        , ( "media_order", Encode.int media.mediaOrder )
        , ( "key", Encode.string media.key )
        , ( "mime", Encode.string media.mime )
        ]


mediaBatchPresignedEncoder :
    List
        { newlyAdded : Bool
        , tweetOrder : Int
        , mediaOrder : Int
        , key : String
        , file : Maybe File
        , mime : String
        }
    -> Encode.Value
mediaBatchPresignedEncoder mediae =
    Encode.list mediaForPresignedEncoder mediae


tweetEncoder : Tweet -> Encode.Value
tweetEncoder tweet =
    Encode.object
        [ ( "tweet", Encode.string tweet.tweet )
        , ( "tweet_order", Encode.int tweet.tweetOrder )
        ]


toEncodedTweets :
    List
        { tweet : String
        , key : String
        , media : List Media
        }
    -> List Tweet
toEncodedTweets tweets =
    List.indexedMap Tuple.pair tweets
        |> List.map
            (\( order, tweet ) ->
                { tweet = tweet.tweet
                , tweetOrder = order + 1
                , media = []
                }
            )


tweetsEncoder :
    { timestamp : Maybe Int
    , media : List PresignedURL
    , tweets :
        List
            { tweet : String
            , key : String
            , media : List Media
            }
    }
    -> Encode.Value
tweetsEncoder model =
    Encode.object
        [ ( "recurring", Encode.bool False )
        , ( "category_id", Encode.null )
        , ( "timestamp", encodeNullable Encode.int model.timestamp )
        , ( "tweets", Encode.list tweetEncoder (toEncodedTweets model.tweets) )
        , ( "media", Encode.list mediaEncoder model.media )
        ]
