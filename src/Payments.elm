module Payments exposing (Transaction, transactionsDecoder)

import Json.Decode as Decode exposing (Decoder, bool, int, list, nullable, string)
import Json.Decode.Pipeline exposing (optional, required)


type alias Transaction =
    { currency : String
    , amount : Int
    , cardBrand : String
    , cardLast4 : String
    , date : Int
    , paid : Bool
    }


transactionDecoder : Decoder Transaction
transactionDecoder =
    Decode.succeed Transaction
        |> required "currency" string
        |> required "amount" int
        |> required "card_brand" string
        |> required "card_last4" string
        |> required "transaction_date" int
        |> required "paid" bool


transactionsDecoder : Decoder (List Transaction)
transactionsDecoder =
    Decode.at [ "data", "transactions" ] (list transactionDecoder)
