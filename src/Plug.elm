module Plug exposing (Plug, plugEncoder, plugsDecoder, singlePlugDecoder)

import Json.Decode as Decode exposing (Decoder, bool, dict, int, list, nullable, string)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode


type alias Plug =
    { id : Int
    , name : String
    , content : String
    }


plugDecoder : Decoder Plug
plugDecoder =
    Decode.succeed Plug
        |> required "id" int
        |> required "name" string
        |> required "content" string


singlePlugDecoder : Decoder Plug
singlePlugDecoder =
    Decode.at [ "data", "plug" ] plugDecoder


plugsDecoder : Decoder (List Plug)
plugsDecoder =
    Decode.at [ "data", "plugs" ] (list plugDecoder)


plugEncoder :
    { name : String
    , content : String
    }
    -> Encode.Value
plugEncoder payload =
    Encode.object
        [ ( "name", Encode.string payload.name )
        , ( "content", Encode.string payload.content )
        ]
