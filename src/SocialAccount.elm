module SocialAccount exposing (SocialAccount, socialAccountsDecoder)

import Json.Decode as Decode exposing (Decoder, bool, int, list, nullable, string)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode


type alias SocialAccount =
    { id : Int
    , description : String
    , platformId : Int
    }


socialAccountDecoder : Decoder SocialAccount
socialAccountDecoder =
    Decode.succeed SocialAccount
        |> required "id" int
        |> required "description" string
        |> required "platform_id" int


socialAccountsDecoder : Decoder (List SocialAccount)
socialAccountsDecoder =
    Decode.at [ "data", "accounts" ] (list socialAccountDecoder)
