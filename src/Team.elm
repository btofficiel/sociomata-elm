module Team exposing (Member, inviteesEncoder, memberDecoder, membersDecoder)

import Json.Decode as Decode exposing (Decoder, bool, int, list, nullable, string)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode


type alias Member =
    { id : Int
    , avatar : Maybe String
    , name : Maybe String
    , email : String
    }


memberDecoder : Decoder Member
memberDecoder =
    Decode.succeed Member
        |> required "id" int
        |> required "avatar" (nullable string)
        |> required "name" (nullable string)
        |> required "email" string


membersDecoder : Decoder (List Member)
membersDecoder =
    Decode.at [ "data", "members" ] (list memberDecoder)


inviteesEncoder : List String -> Encode.Value
inviteesEncoder invitees =
    Encode.object
        [ ( "emails", Encode.list Encode.string invitees )
        ]
