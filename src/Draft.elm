module Draft exposing (Draft, draftsDecoder)

import Json.Decode as Decode exposing (Decoder, bool, dict, int, list, nullable, string)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Post exposing (PostId, postIdDecoder)


type alias Draft =
    { id : PostId
    , description : String
    }


draftDecoder : Decoder Draft
draftDecoder =
    Decode.succeed Draft
        |> required "id" postIdDecoder
        |> required "description" string


draftsDecoder : Decoder (List Draft)
draftsDecoder =
    Decode.at [ "data", "drafts" ] (list draftDecoder)
