module Tweet exposing (Tweet, tweetsDecoder)

import Json.Decode as Decode exposing (Decoder, bool, int, list, string)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Url.Parser exposing (Parser)


type alias Tweet =
    { tweet : String
    , tweetOrder : Int
    }


tweetDecoder : Decoder Tweet
tweetDecoder =
    Decode.succeed Tweet
        |> required "tweet" string
        |> required "tweet_order" int


tweetsDecoder : Decoder (List Tweet)
tweetsDecoder =
    list tweetDecoder
