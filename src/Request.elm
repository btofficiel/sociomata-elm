module Request exposing (expectJson)

import Http
import Json.Decode as D


expectJson : (Result Http.Error a -> msg) -> D.Decoder a -> Http.Expect msg
expectJson toMsg decoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    case metadata.statusCode of
                        401 ->
                            Err (Http.BadStatus 401)

                        502 ->
                            Err (Http.BadBody "We're sorry. Our systems are temporarily down.")

                        _ ->
                            case D.decodeString (D.field "message" D.string) body of
                                Ok message ->
                                    Err (Http.BadBody message)

                                Err _ ->
                                    Err (Http.BadBody "Oop! Something unexpected happened.")

                Http.GoodStatus_ metadata body ->
                    case D.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (Http.BadBody (D.errorToString err))
