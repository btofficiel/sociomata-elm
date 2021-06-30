module MessageBanner exposing (Message(..), MessageBanner, fadeMessage)

import Process
import Task


type Message
    = Loading String
    | Success String
    | Failure String


type alias MessageBanner =
    Maybe Message


fadeMessage : msg -> Cmd msg
fadeMessage message =
    Process.sleep 3000
        |> Task.perform (\_ -> message)
