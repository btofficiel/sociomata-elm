port module Api exposing (base64FromUrl, deleteToken)

import Array


port deleteToken : () -> Cmd msg


base64FromUrl : Maybe String -> Maybe String
base64FromUrl url =
    let
        base64 =
            case url of
                Just dataUrl ->
                    String.split "," dataUrl
                        |> Array.fromList
                        |> Array.get 1

                Nothing ->
                    Nothing
    in
    base64
