module Timezone exposing (Timezone, timezonesDecoder)

import Json.Decode as Decode exposing (Decoder, bool, int, list, nullable, string)
import Json.Decode.Pipeline exposing (optional, required)


type alias Timezone =
    { id : Int
    , timezone : String
    , offsetMins : Int
    }


timezoneDecoder : Decoder Timezone
timezoneDecoder =
    Decode.succeed Timezone
        |> required "id" int
        |> required "timezone" string
        |> required "offset_mins" int


timezonesDecoder : Decoder (List Timezone)
timezonesDecoder =
    Decode.at [ "data", "timezones" ] (list timezoneDecoder)
