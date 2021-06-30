module DateTime exposing (Offset, getNewTime, posixToDate, posixToSeconds, secondsToPosix, viewTime)

import Task
import Time exposing (Month(..), Posix, Zone, toDay, toHour, toMinute, toMonth, toYear, utc)


type alias Offset =
    Int


monthToString : Month -> String
monthToString month =
    case month of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"


getNewTime : (Posix -> msg) -> Cmd msg
getNewTime message =
    Task.perform message Time.now


posixToSeconds : Posix -> Int
posixToSeconds posix =
    Time.posixToMillis posix // 1000


secondsToPosix : Int -> Posix
secondsToPosix seconds =
    Time.millisToPosix (seconds * 1000)


getTimePeriod : Int -> String
getTimePeriod hour =
    if hour > 11 then
        "PM"

    else
        "AM"


convert24hTo12h : Int -> Int
convert24hTo12h hour =
    if hour > 12 then
        hour - 12

    else
        hour


viewTime : Int -> Zone -> String
viewTime timestamp timezone =
    posixToTime (secondsToPosix timestamp) timezone


posixToTime : Posix -> Zone -> String
posixToTime timestamp timezone =
    let
        hourIn24hFormat =
            toHour timezone timestamp

        hour =
            String.fromInt (convert24hTo12h hourIn24hFormat)

        minutes =
            String.fromInt (toMinute timezone timestamp)

        timePeriod =
            getTimePeriod hourIn24hFormat
    in
    String.concat [ hour, ":", minutes, timePeriod ]


posixToDate : Posix -> String
posixToDate timestamp =
    let
        day =
            String.fromInt (toDay (Time.customZone 330 []) timestamp)

        month =
            monthToString (toMonth utc timestamp)

        year =
            String.fromInt (toYear utc timestamp)
    in
    String.concat [ day, " ", month, " ", year ]
