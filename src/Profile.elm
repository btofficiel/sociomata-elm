module Profile exposing (Avatar, Config, Profile, configDecoder, getAvatar, getOffset)

import Json.Decode as Decode exposing (Decoder, bool, int, list, nullable, string)
import Json.Decode.Pipeline exposing (optional, required)
import RemoteData exposing (WebData)


type alias Profile =
    { name : String
    , timezoneId : Int
    , avatar : Avatar
    , offset : Int
    }


type alias Config =
    { profile : Maybe Profile
    , email : String
    , twitterConnected : Bool
    }


type alias Avatar =
    Maybe String


profileDecoder : Decoder Profile
profileDecoder =
    Decode.succeed Profile
        |> required "name" string
        |> required "defaulttimezone" int
        |> required "avatar" (nullable string)
        |> required "offset_mins" int


configDecoder : Decoder Config
configDecoder =
    Decode.at [ "data" ] <|
        (Decode.succeed Config
            |> required "profile" (nullable profileDecoder)
            |> required "email" string
            |> required "twitter" bool
        )


getOffset : WebData Config -> Int
getOffset config =
    case config of
        RemoteData.Success conf ->
            case conf.profile of
                Just profile ->
                    profile.offset

                Nothing ->
                    0

        _ ->
            0


getAvatar : WebData Config -> Maybe String
getAvatar config =
    case config of
        RemoteData.Success conf ->
            case conf.profile of
                Just profile ->
                    profile.avatar

                Nothing ->
                    Nothing

        _ ->
            Nothing
