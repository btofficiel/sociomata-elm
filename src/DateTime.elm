module DateTime exposing (Offset, getNewTime, posixToDate, posixToSeconds, secondsToPosix, timestampToHumanTime, viewTime)

import Task
import Time exposing (Month(..), Posix, Zone, toDay, toHour, toMinute, toMonth, toYear, utc)


type alias Offset =
    Int


monthToNum : Month -> Int
monthToNum month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


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
            "July"

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


posixToDate : Posix -> String -> Zone -> String
posixToDate timestamp currentYear zone =
    let
        day =
            String.fromInt (toDay zone timestamp)

        month =
            monthToString (toMonth zone timestamp)

        year =
            String.fromInt (toYear zone timestamp)
    in
    if year == currentYear then
        String.concat [ day, " ", month ]

    else
        String.concat [ day, " ", month, " ", year ]


dateNumToString : Int -> String
dateNumToString datenum =
    case datenum > 9 of
        True ->
            String.fromInt datenum

        False ->
            String.concat [ "0", String.fromInt datenum ]


timestampToHumanTime : Int -> Zone -> String
timestampToHumanTime ts zone =
    let
        millis =
            ts * 1000

        posix =
            Time.millisToPosix millis

        year =
            toYear zone posix
                |> String.fromInt

        month =
            toMonth zone posix
                |> monthToNum
                |> dateNumToString

        day =
            toDay zone posix
                |> dateNumToString

        hour =
            toHour zone posix
                |> dateNumToString

        minute =
            toMinute zone posix
                |> dateNumToString
    in
    String.concat [ year, "-", month, "-", day, "T", hour, ":", minute ]
