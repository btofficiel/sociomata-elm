module Profile exposing (Avatar, Config, Profile, configDecoder, displayOnboardingMessage, getAvatar, getOffset)

import Json.Decode as Decode exposing (Decoder, bool, int, list, nullable, string)
import Json.Decode.Pipeline exposing (optional, required)
import MessageBanner as Message exposing (MessageBanner)
import RemoteData exposing (WebData)


type alias Profile =
    { name : String
    , timezoneId : Int
    , avatar : Avatar
    , offset : Int
    }


type alias AccountDetails =
    { id : Int
    , name : String
    , maxUsers : Int
    , maxAccounts : Int
    , planId : Int
    , trialPeriod : Int
    , trialActive : Bool
    , subPeriod : Int
    , subActive : Bool
    }


type alias Config =
    { profile : Maybe Profile
    , email : String
    , socialAccounts : Bool
    , isAdmin : Bool
    , account : AccountDetails
    }


type alias Avatar =
    Maybe String


accountDecoder : Decoder AccountDetails
accountDecoder =
    Decode.succeed AccountDetails
        |> required "id" int
        |> required "name" string
        |> required "max_users" int
        |> required "max_accounts" int
        |> required "plan_id" int
        |> required "trial_period" int
        |> required "trial_active" bool
        |> optional "current_period_end" int 0
        |> optional "sub_active" bool False


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
            |> required "social_accounts" bool
            |> required "is_admin" bool
            |> required "account" accountDecoder
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


displayOnboardingMessage : WebData Config -> MessageBanner
displayOnboardingMessage conf =
    case conf of
        RemoteData.Success config ->
            case config.socialAccounts of
                True ->
                    Nothing

                False ->
                    Just (Message.Onboarding "Get started by connecting your twitter account in settings")

        _ ->
            Nothing
