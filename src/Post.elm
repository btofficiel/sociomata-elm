module Post exposing (DictPost, Post, postsQueueDecoder, timestampToInt)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, bool, dict, int, list, nullable, string)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode


type alias Post =
    { id : PostId
    , timestamp : Timestamp
    , recurring : Bool
    , description : String
    , categoryId : Maybe CategoryId
    , status : Int
    }


type alias DictPost =
    Dict String (List Post)


type PostId
    = PostId Int


type Timestamp
    = Timestamp Int


type CategoryId
    = CategoryId Int


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


postDecoder : Decoder Post
postDecoder =
    Decode.succeed Post
        |> required "id" postIdDecoder
        |> required "timestamp" timestampDecoder
        |> required "recurring" bool
        |> required "description" string
        |> required "category_id" (nullable categoryIdDecoder)
        |> required "status" int


postsQueueDecoder : Decoder (Dict String (List Post))
postsQueueDecoder =
    Decode.at [ "data", "posts" ] (dict (list postDecoder))


emptyPost : Post
emptyPost =
    { id = PostId -1
    , timestamp = Timestamp -1
    , recurring = False
    , description = ""
    , categoryId = Just (CategoryId -1)
    , status = 1
    }


timestampToInt : Timestamp -> Int
timestampToInt (Timestamp ts) =
    ts
