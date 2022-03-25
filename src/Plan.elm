module Plan exposing (Plan, plansDecoder)

import Json.Decode as Decode exposing (Decoder, bool, int, list, nullable, string)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode


type alias Plan =
    { id : Int
    , name : String
    , priceId : String
    , price : Int
    , symbol : String
    }


planDecoder : Decoder Plan
planDecoder =
    Decode.succeed Plan
        |> required "id" int
        |> required "name" string
        |> required "price_id" string
        |> required "price" int
        |> required "symbol" string


plansDecoder : Decoder (List Plan)
plansDecoder =
    Decode.at [ "data", "plans" ] (list planDecoder)
