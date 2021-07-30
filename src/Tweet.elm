module Tweet exposing (Tweet, encodeNullable, tweetsDecoder, tweetsEncoder)

import Json.Decode as Decode exposing (Decoder, bool, int, list, string)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Tuple
import Url.Parser exposing (Parser)


type alias Tweet =
    { tweet : String
    , tweetOrder : Int
    }


encodeNullable : (value -> Encode.Value) -> Maybe value -> Encode.Value
encodeNullable valueEncoder maybeValue =
    case maybeValue of
        Just value ->
            valueEncoder value

        Nothing ->
            Encode.null


tweetDecoder : Decoder Tweet
tweetDecoder =
    Decode.succeed Tweet
        |> required "tweet" string
        |> required "tweet_order" int


tweetsDecoder : Decoder (List Tweet)
tweetsDecoder =
    list tweetDecoder


tweetEncoder : Tweet -> Encode.Value
tweetEncoder tweet =
    Encode.object
        [ ( "tweet", Encode.string tweet.tweet )
        , ( "tweet_order", Encode.int tweet.tweetOrder )
        ]


toEncodedTweets : List { tweet : String, key : String } -> List Tweet
toEncodedTweets tweets =
    List.indexedMap Tuple.pair tweets
        |> List.map (\( order, tweet ) -> { tweet = tweet.tweet, tweetOrder = order + 1 })


tweetsEncoder : { timestamp : Maybe Int, tweets : List { tweet : String, key : String } } -> Encode.Value
tweetsEncoder model =
    Encode.object
        [ ( "recurring", Encode.bool False )
        , ( "category_id", Encode.null )
        , ( "timestamp", encodeNullable Encode.int model.timestamp )
        , ( "tweets", Encode.list tweetEncoder (toEncodedTweets model.tweets) )
        ]
