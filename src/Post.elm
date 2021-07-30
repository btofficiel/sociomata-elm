module Post exposing (DictPost, Post, PostId(..), compareById, postIdDecoder, postsQueueDecoder, pseudoPost, timestampToInt, twitterPostDecoder)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, bool, dict, int, list, nullable, string)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Tweet exposing (Tweet, tweetsDecoder)


type alias Post =
    { id : PostId
    , timestamp : Timestamp
    , recurring : Bool
    , description : String
    , categoryId : Maybe CategoryId
    , status : Int
    , tweets : List Tweet
    }


type alias DictPost =
    Dict String (List Post)


type PostId
    = PostId Int


type Timestamp
    = Timestamp Int


type CategoryId
    = CategoryId Int


type PostType
    = General
    | Twitter


categoryIdDecoder : Decoder CategoryId
categoryIdDecoder =
    Decode.map CategoryId int


categoryIdMaybeDecoder : Decoder (Maybe CategoryId)
categoryIdMaybeDecoder =
    Decode.map Just categoryIdDecoder


timestampDecoder : Decoder Timestamp
timestampDecoder =
    Decode.map Timestamp int


postIdDecoder : Decoder PostId
postIdDecoder =
    Decode.map PostId int


twitterPostDecoder : Decoder Post
twitterPostDecoder =
    Decode.at [ "data", "post" ] (postDecoder Twitter)


generalPostDecoder : Decoder Post
generalPostDecoder =
    postDecoder General


postDecoder : PostType -> Decoder Post
postDecoder postType =
    let
        baseDecoder =
            Decode.succeed Post
                |> required "id" postIdDecoder
                |> required "timestamp" timestampDecoder
                |> required "recurring" bool
                |> required "description" string
                |> required "category_id" (nullable categoryIdDecoder)
                |> required "status" int

        appliedDecoder =
            case postType of
                General ->
                    baseDecoder
                        |> optional "tweets" tweetsDecoder []

                Twitter ->
                    baseDecoder
                        |> required "tweets" tweetsDecoder
    in
    appliedDecoder


postsQueueDecoder : Decoder (Dict String (List Post))
postsQueueDecoder =
    Decode.at [ "data", "posts" ] (dict (list generalPostDecoder))


timestampToInt : Timestamp -> Int
timestampToInt (Timestamp ts) =
    ts


postIdToInt : PostId -> Int
postIdToInt (PostId id) =
    id


compareById : PostId -> PostId -> Bool
compareById (PostId pid1) (PostId pid2) =
    pid1 == pid2


pseudoPost : Post
pseudoPost =
    { id = PostId -1
    , timestamp = Timestamp -1
    , recurring = False
    , description = ""
    , categoryId = Nothing
    , status = -1
    , tweets = []
    }
