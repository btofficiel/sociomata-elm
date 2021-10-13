module Media exposing (Media, emptyMedia, mediaDecoder)

import Json.Decode as Decode exposing (Decoder, bool, int, list, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)


type alias Media =
    { key : String
    , bucket : String
    , algorithm : String
    , cred : String
    , date : String
    , policy : String
    , signature : String
    }


emptyMedia : Media
emptyMedia =
    { key = ""
    , bucket = ""
    , algorithm = ""
    , cred = ""
    , date = ""
    , policy = ""
    , signature = ""
    }


mediaDecoder : Decoder Media
mediaDecoder =
    Decode.succeed Media
        |> required "key" string
        |> required "bucket" string
        |> required "X-Amz-Algorithm" string
        |> required "X-Amz-Credential" string
        |> required "X-Amz-Date" string
        |> required "Policy" string
        |> required "X-Amz-Signature" string


mediaeDecoder : Decoder (List Media)
mediaeDecoder =
    list mediaDecoder
