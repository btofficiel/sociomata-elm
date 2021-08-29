module Tweet exposing (Media, Tweet, encodeNullable, tweetsDecoder, tweetsEncoder)

import Api
import Json.Decode as Decode exposing (Decoder, bool, int, list, string)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Tuple
import Url.Parser exposing (Parser)


type alias Tweet =
    { tweet : String
    , tweetOrder : Int
    , media : List Media
    }


type alias Media =
    { newlyAdded : Bool
    , url : String
    }


encodeNullable : (value -> Encode.Value) -> Maybe value -> Encode.Value
encodeNullable valueEncoder maybeValue =
    case maybeValue of
        Just value ->
            valueEncoder value

        Nothing ->
            Encode.null


mediaDecoder : Decoder Media
mediaDecoder =
    Decode.succeed Media
        |> required "newly_added" bool
        |> required "url" string


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


mediaEncoder : Media -> Encode.Value
mediaEncoder media =
    Encode.object
        [ ( "newly_added", Encode.bool media.newlyAdded )
        , ( "url"
          , Encode.string
                (case media.newlyAdded of
                    True ->
                        Maybe.withDefault "" (Api.base64FromUrl (Just media.url))

                    False ->
                        media.url
                )
          )
        ]


tweetEncoder : Tweet -> Encode.Value
tweetEncoder tweet =
    Encode.object
        [ ( "tweet", Encode.string tweet.tweet )
        , ( "tweet_order", Encode.int tweet.tweetOrder )
        , ( "media", Encode.list mediaEncoder tweet.media )
        ]


toEncodedTweets : List { tweet : String, key : String, media : List Media } -> List Tweet
toEncodedTweets tweets =
    List.indexedMap Tuple.pair tweets
        |> List.map
            (\( order, tweet ) ->
                { tweet = tweet.tweet
                , tweetOrder = order + 1
                , media = tweet.media
                }
            )


tweetsEncoder : { timestamp : Maybe Int, tweets : List { tweet : String, key : String, media : List Media } } -> Encode.Value
tweetsEncoder model =
    Encode.object
        [ ( "recurring", Encode.bool False )
        , ( "category_id", Encode.null )
        , ( "timestamp", encodeNullable Encode.int model.timestamp )
        , ( "tweets", Encode.list tweetEncoder (toEncodedTweets model.tweets) )
        ]
