module Page.Settings exposing (Model, Msg(..), init, update, view)

import Api
import Browser.Navigation as Nav
import DateTime
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, img, input, li, option, section, select, span, table, td, text, th, tr, ul)
import Html.Attributes exposing (class, placeholder, selected, src, style, type_, value)
import Html.Events exposing (onClick, onInput, onMouseOut, onMouseOver)
import Http
import Json.Decode as Decode exposing (Decoder, string)
import Json.Encode as Encode
import MessageBanner as Message exposing (MessageBanner)
import Page.Loading
import Payments exposing (Transaction, transactionsDecoder)
import Profile exposing (Config, configDecoder)
import RemoteData exposing (WebData)
import Request
import Route exposing (Token)
import Task
import Time
import Timezone exposing (Timezone, timezonesDecoder)
import Tweet as T


type alias Model =
    { name : String
    , email : String
    , password : String
    , newPassword : String
    , timezone : Int
    , message : MessageBanner
    , subView : View
    , timezones : WebData (List Timezone)
    , transactions : WebData (List Transaction)
    , avatar : Maybe String
    , twitter : AccountConnection
    }


type alias AccountConnection =
    { connected : Bool
    , showDisconnectButon : Bool
    }


type Msg
    = PickAvatar
    | FadeMessage
    | GotAvatar File
    | GotTimezones (WebData (List Timezone))
    | GotTransactions (WebData (List Transaction))
    | GotConnectToken (WebData String)
    | GotAvatarURL String
    | GotUpdatedProfile (WebData Config)
    | GotUpdatedAccount (Result Http.Error ())
    | ChangeSubView View
    | StoreTimezone String
    | StoreName String
    | StoreEmail String
    | StoreCurrentPassword String
    | StoreNewPassword String
    | RequestConnectToken
    | DisconnectTwitter
    | GotDeletedAccount (Result Http.Error ())
    | UpdateProfile
    | UpdateAccount
    | ToggleDisconnectButton Bool


type View
    = AccountSettings
    | ProfileSettings
    | PaymentSettings
    | SocialAccountsSettings


init : Token -> ( Model, Cmd Msg )
init token =
    let
        model =
            { name = ""
            , email = ""
            , password = ""
            , newPassword = ""
            , timezone = 80
            , message = Nothing
            , subView = ProfileSettings
            , timezones = RemoteData.Loading
            , transactions = RemoteData.Loading
            , avatar = Nothing
            , twitter =
                { connected = False
                , showDisconnectButon = False
                }
            }
    in
    ( model, Cmd.batch [ fetchTimezones token, fetchTransactions token ] )


accountEncoder : Model -> Encode.Value
accountEncoder model =
    Encode.object
        [ ( "email", Encode.string model.email )
        , ( "password", Encode.string model.password )
        , ( "new_password", Encode.string model.newPassword )
        ]


profileEncoder : Model -> Encode.Value
profileEncoder model =
    Encode.object
        [ ( "name", Encode.string model.name )
        , ( "timezone", Encode.int model.timezone )
        , ( "avatar", T.encodeNullable Encode.string (Api.base64FromUrl model.avatar) )
        ]


updateAccount : Model -> Token -> Cmd Msg
updateAccount model token =
    Http.request
        { method = "PUT"
        , headers = [ Http.header "Authorization" token ]
        , url = "/api/account"
        , body = Http.jsonBody (profileEncoder model)
        , expect = Http.expectWhatever GotUpdatedAccount
        , timeout = Nothing
        , tracker = Nothing
        }


updateProfile : Model -> Token -> Cmd Msg
updateProfile model token =
    Http.request
        { method = "PUT"
        , headers = [ Http.header "Authorization" token ]
        , url = "/api/profile"
        , body = Http.jsonBody (profileEncoder model)
        , expect = Request.expectJson (RemoteData.fromResult >> GotUpdatedProfile) configDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


fetchTransactions : Token -> Cmd Msg
fetchTransactions token =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" token ]
        , url = "/api/transactions"
        , body = Http.emptyBody
        , expect = Request.expectJson (RemoteData.fromResult >> GotTransactions) transactionsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


fetchTimezones : Token -> Cmd Msg
fetchTimezones token =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" token ]
        , url = "/api/timezones"
        , body = Http.emptyBody
        , expect = Request.expectJson (RemoteData.fromResult >> GotTimezones) timezonesDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


fetchConnectToken token =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" token ]
        , url = "/api/connect"
        , body = Http.emptyBody
        , expect = Request.expectJson (RemoteData.fromResult >> GotConnectToken) (Decode.at [ "token" ] string)
        , timeout = Nothing
        , tracker = Nothing
        }


disconnectTwitter : Token -> Cmd Msg
disconnectTwitter token =
    let
        decoder =
            Decode.succeed ()

        url =
            "/api/connect/twitter"
    in
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "Authorization" token ]
        , url = url
        , body = Http.emptyBody
        , expect = Request.expectJson GotDeletedAccount decoder
        , timeout = Nothing
        , tracker = Nothing
        }


changeSettingsMenuClass : View -> View -> String
changeSettingsMenuClass subview selection =
    case subview == selection of
        True ->
            "active"

        False ->
            ""


calculateAccountLimit : Int -> Float -> String
calculateAccountLimit accounts total =
    let
        progress =
            toFloat accounts
                / total

        width =
            progress
                |> (*) 8
                |> String.fromFloat
    in
    width


settingsMenu : View -> Html Msg
settingsMenu subview =
    ul [ class "settings-menu" ]
        [ {---li
            [ changeSettingsMenuClass subview AccountSettings
                |> class
            , onClick (ChangeSubView AccountSettings)
            ]
            [ text "Account settings" ]
            ,--}
          li
            [ changeSettingsMenuClass subview ProfileSettings
                |> class
            , onClick (ChangeSubView ProfileSettings)
            ]
            [ text "Profile settings" ]
        , li
            [ changeSettingsMenuClass subview PaymentSettings
                |> class
            , onClick (ChangeSubView PaymentSettings)
            ]
            [ text "Payment settings" ]
        , li
            [ changeSettingsMenuClass subview SocialAccountsSettings
                |> class
            , onClick (ChangeSubView SocialAccountsSettings)
            ]
            [ text "Social Accounts" ]
        ]


viewTimezones : ( WebData (List Timezone), Int ) -> List (Html Msg)
viewTimezones ( timezones, currentTimezone ) =
    case timezones of
        RemoteData.Success actualTimezones ->
            List.map
                (\tz ->
                    case tz.id == currentTimezone of
                        True ->
                            option [ value (String.fromInt tz.id), selected True ] [ text tz.timezone ]

                        False ->
                            option [ value (String.fromInt tz.id) ] [ text tz.timezone ]
                )
                actualTimezones

        _ ->
            [ option [] [] ]


accountSettingsView : Model -> Html Msg
accountSettingsView model =
    div [ class "account-settings" ]
        [ span [] [ text "Email" ]
        , input
            [ type_ "email"
            , placeholder "Enter your email"
            , onInput StoreEmail
            , value model.email
            ]
            []
        , span
            []
            [ text "Current Password" ]
        , input
            [ type_ "password"
            , placeholder "Enter your current password"
            , onInput StoreCurrentPassword
            , value model.password
            ]
            []
        , span [] [ text "New Password" ]
        , input
            [ type_ "password"
            , placeholder "Enter new password"
            , onInput StoreNewPassword
            , value model.newPassword
            ]
            []
        , button [] [ text "Update account" ]
        ]


twitterConnectionButton : AccountConnection -> Html Msg
twitterConnectionButton connection =
    case connection.connected of
        True ->
            span
                [ class "disconnection-btns"
                , onMouseOver (ToggleDisconnectButton True)
                , onMouseOut (ToggleDisconnectButton False)
                ]
                [ case connection.showDisconnectButon of
                    True ->
                        button
                            [ onClick DisconnectTwitter
                            ]
                            [ text "Disconnect" ]

                    False ->
                        div
                            [ class "account-connected"
                            ]
                            [ text "Connected" ]
                ]

        False ->
            button [ onClick RequestConnectToken ] [ text "Connect" ]


profileSettingsView : Model -> Html Msg
profileSettingsView model =
    div [ class "profile-settings" ]
        [ div []
            [ span [] [ text "Name" ]
            , input [ onInput StoreName, placeholder "Enter your name", value model.name ] []
            , span [] [ text "Default Timezone" ]
            , viewTimezones ( model.timezones, model.timezone )
                |> select [ onInput StoreTimezone ]
            , span [] [ text "Twitter Account" ]
            , twitterConnectionButton model.twitter
            , button [ onClick UpdateProfile ] [ text "Update profile" ]
            ]
        , div [ class "avatar-settings-container" ]
            [ div [ class "avatar-container" ]
                [ img [ src (Page.Loading.getAvatarURL model.avatar), class "settings-avatar" ] []
                , span [ class "update-avatar", onClick PickAvatar ] [ text "Update avatar" ]
                ]
            ]
        ]


socialAccountSettings : Model -> Html Msg
socialAccountSettings model =
    div [ class "social-account-settings" ]
        [ div [ class "add-account" ]
            [ span [ class "social-header" ]
                [ text "Connect a new account" ]
            , span [ class "twitter-counter" ]
                [ span []
                    [ text
                        (String.concat
                            [ String.fromInt 1
                            , "/4"
                            ]
                        )
                    ]
                , span [ class "tweet-count-indicator" ]
                    [ span
                        [ class "tweet-count-progress"
                        , style "width"
                            (String.concat
                                [ calculateAccountLimit 1 4
                                , "rem"
                                ]
                            )
                        ]
                        []
                    ]
                ]
            ]
        , div [ class "platforms" ]
            [ div [ class "platform" ]
                [ img [ class "social-logo", src "/images/twitter.png" ] []
                , span [ class "platform-name" ] [ text "Twitter" ]
                , span [ class "platform-account" ] [ text "Profile" ]
                , span [ class "button" ]
                    [ button [] [ text "Connect" ]
                    ]
                ]
            , div [ class "platform" ]
                [ img [ class "social-logo", src "/images/fb.png" ] []
                , span [ class "platform-name" ] [ text "Facebook" ]
                , span [ class "platform-account" ] [ text "Page or Group" ]
                , span [ class "button" ]
                    [ button [] [ text "Connect" ]
                    ]
                ]
            , div [ class "platform" ]
                [ img [ class "social-logo", src "/images/ig.png" ] []
                , span [ class "platform-name" ] [ text "Instagram" ]
                , span [ class "platform-account" ] [ text "Business account" ]
                , span [ class "button" ]
                    [ button [] [ text "Connect" ]
                    ]
                ]
            , div [ class "platform" ]
                [ img [ class "social-logo", src "/images/linkedin.png" ] []
                , span [ class "platform-name" ] [ text "LinkedIn" ]
                , span [ class "platform-account" ] [ text "Page or Profile" ]
                , span [ class "button" ]
                    [ button [] [ text "Connect" ]
                    ]
                ]
            ]
        ]


paymentSettingsView : Model -> Html Msg
paymentSettingsView model =
    case model.transactions of
        RemoteData.Success transactions ->
            div [ class "payment-settings" ]
                [ div [ class "heading" ] [ text "Transactions" ]
                , div [ class "heading" ] [ text "Current Plan" ]
                , table []
                    ([ tr []
                        [ th [] [ text "Amount" ]
                        , th [] [ text "Date" ]
                        , th [] [ text "Card" ]
                        , th [] [ text "Status" ]
                        ]
                     ]
                        ++ List.map
                            (\t ->
                                tr []
                                    [ td []
                                        [ text
                                            ((case t.currency of
                                                "usd" ->
                                                    "$"

                                                _ ->
                                                    "₹"
                                             )
                                                ++ String.fromInt t.amount
                                            )
                                        ]
                                    , td []
                                        [ text
                                            (DateTime.posixToDate (DateTime.secondsToPosix t.date) "0" (Time.customZone model.timezone []))
                                        ]
                                    , td [] [ text (t.cardBrand ++ " " ++ t.cardLast4) ]
                                    , td []
                                        [ text
                                            (case t.paid of
                                                True ->
                                                    "Paid"

                                                False ->
                                                    "Failed"
                                            )
                                        ]
                                    ]
                            )
                            transactions
                    )
                , span [] [ div [ class "plan" ] [ text "Creator Plan" ] ]
                ]

        RemoteData.NotAsked ->
            Page.Loading.body

        RemoteData.Loading ->
            Page.Loading.body

        RemoteData.Failure (Http.BadBody err) ->
            Page.Loading.emptyState 80 err

        RemoteData.Failure Http.NetworkError ->
            Page.Loading.emptyState 80 "Oops! Looks like there is some problem with your network."

        _ ->
            Page.Loading.emptyState 80 "Oops! This looks like an unknown error. Contact support"



{--
    div [ class "payment-settings" ]
        [ table []
            [ tr []
                [ th [] [ text "Amount" ]
                , th [] [ text "Date" ]
                , th [] [ text "Card" ]
                , th [] [ text "Status" ]
                ]
            ]
        , div [ class "plan" ] []
        ]
    --}


viewSettingsForms : Model -> Html Msg
viewSettingsForms model =
    case model.subView of
        AccountSettings ->
            accountSettingsView model

        ProfileSettings ->
            profileSettingsView model

        PaymentSettings ->
            paymentSettingsView model

        SocialAccountsSettings ->
            socialAccountSettings model


body : Model -> Html Msg
body model =
    case model.timezones of
        RemoteData.NotAsked ->
            Page.Loading.body

        RemoteData.Loading ->
            Page.Loading.body

        _ ->
            div [ class "container" ]
                [ section [ class "panels" ]
                    [ section [ class "subheader" ]
                        [ span [] [ text "Settings" ]
                        ]
                    , section [ class "dual-panel" ]
                        [ div [ class "filters" ]
                            [ span [] [ text "Menu" ]
                            , span [ class "filter-box" ]
                                [ settingsMenu model.subView
                                ]
                            ]
                        , div [ class "settings-board" ]
                            [ viewSettingsForms model ]
                        ]
                    ]
                ]


view : Model -> WebData Config -> Html Msg
view model config =
    div []
        [ Message.viewMessage model.message
        , body model

        {---
        , button
            [ onClick PickAvatar ]
            [ text "Pick Avatar" ]
        , div [] [ text (Debug.toString model) ]
        , img [ src (avatarToString model.avatar), class "settings-avatar" ] []
        ---}
        ]


update : Msg -> Model -> Token -> ( Model, Cmd Msg )
update msg model token =
    case msg of
        DisconnectTwitter ->
            ( { model
                | message = Just Message.Loading
              }
            , disconnectTwitter token
            )

        ToggleDisconnectButton bool ->
            let
                twitter =
                    model.twitter

                new_twitter =
                    { twitter
                        | showDisconnectButon = bool
                    }
            in
            ( { model | twitter = new_twitter }, Cmd.none )

        PickAvatar ->
            ( model
            , Select.file [ "image/jpeg", "image/jpg", "image/png" ] GotAvatar
            )

        FadeMessage ->
            ( { model | message = Nothing }, Cmd.none )

        GotConnectToken response ->
            case response of
                RemoteData.Success token_ ->
                    ( model, String.concat [ "/api/connect/twitter?token=", token_ ] |> Nav.load )

                _ ->
                    ( model, Cmd.none )

        GotTimezones response ->
            case response of
                RemoteData.Success _ ->
                    ( { model | timezones = response }, Cmd.none )

                RemoteData.Failure err ->
                    case err of
                        Http.BadStatus 401 ->
                            ( model
                            , Cmd.batch
                                [ Api.deleteToken ()
                                , Nav.load "/login"
                                ]
                            )

                        _ ->
                            ( { model | timezones = response }, Cmd.none )

                _ ->
                    ( { model | timezones = response }, Cmd.none )

        GotTransactions response ->
            case response of
                RemoteData.Success _ ->
                    ( { model | transactions = response }, Cmd.none )

                RemoteData.Failure err ->
                    case err of
                        Http.BadStatus 401 ->
                            ( model
                            , Cmd.batch
                                [ Api.deleteToken ()
                                , Nav.load "/login"
                                ]
                            )

                        _ ->
                            ( { model | transactions = response }, Cmd.none )

                _ ->
                    ( { model | transactions = response }, Cmd.none )

        GotDeletedAccount (Ok _) ->
            let
                new_twitter =
                    { connected = False
                    , showDisconnectButon = False
                    }
            in
            ( { model
                | message = Just (Message.Success "Your twitter account has been disconnected")
                , twitter = new_twitter
              }
            , Message.fadeMessage FadeMessage
            )

        GotDeletedAccount (Err error) ->
            case error of
                Http.BadBody err ->
                    ( { model | message = Just (Message.Failure err) }, Message.fadeMessage FadeMessage )

                Http.NetworkError ->
                    let
                        err =
                            "Oops! Looks like there is some problem with your network."
                    in
                    ( { model | message = Just (Message.Failure err) }, Message.fadeMessage FadeMessage )

                Http.BadStatus 401 ->
                    ( model
                    , Cmd.batch
                        [ Api.deleteToken ()
                        , Nav.load "/login"
                        ]
                    )

                _ ->
                    let
                        err =
                            "Oops! Something bad happened, please try reloading the app"
                    in
                    ( { model | message = Just (Message.Failure err) }, Message.fadeMessage FadeMessage )

        GotAvatar file ->
            let
                isFileTypeAlllowed =
                    [ "image/jpeg", "image/png", "image/jpg" ]
                        |> List.member (File.mime file)
            in
            case isFileTypeAlllowed of
                True ->
                    case File.size file > 2097152 of
                        True ->
                            ( { model
                                | message = Just (Message.Failure "Please upload an image which is under 2MB in size")
                              }
                            , Message.fadeMessage FadeMessage
                            )

                        False ->
                            ( model, Task.perform GotAvatarURL (File.toUrl file) )

                False ->
                    ( { model
                        | message = Just (Message.Failure "Please use a JPG or PNG file format for avatar")
                      }
                    , Message.fadeMessage FadeMessage
                    )

        GotAvatarURL url ->
            ( { model | avatar = Just url }, Cmd.none )

        ChangeSubView subview ->
            ( { model | subView = subview }, Cmd.none )

        StoreTimezone timezoneId ->
            case String.toInt timezoneId of
                Just tz ->
                    ( { model | timezone = tz }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        StoreName name ->
            ( { model | name = name }, Cmd.none )

        StoreEmail email ->
            ( { model | email = email }, Cmd.none )

        StoreCurrentPassword currentpw ->
            ( { model | password = currentpw }, Cmd.none )

        StoreNewPassword pw ->
            ( { model | newPassword = pw }, Cmd.none )

        RequestConnectToken ->
            ( model, fetchConnectToken token )

        UpdateProfile ->
            ( { model
                | message = Just Message.Loading
              }
            , updateProfile model token
            )

        UpdateAccount ->
            ( model, updateProfile model token )

        GotUpdatedProfile result ->
            case result of
                RemoteData.Success _ ->
                    let
                        message =
                            "Yes! your profile has been updated"
                    in
                    ( { model
                        | message = Just (Message.Success message)
                      }
                    , Message.fadeMessage FadeMessage
                    )

                RemoteData.Failure (Http.BadBody err) ->
                    ( { model | message = Just (Message.Failure err) }, Message.fadeMessage FadeMessage )

                RemoteData.Failure Http.NetworkError ->
                    let
                        err =
                            "Oops! Looks like there is some problem with your network."
                    in
                    ( { model | message = Just (Message.Failure err) }, Message.fadeMessage FadeMessage )

                RemoteData.Failure (Http.BadStatus 401) ->
                    ( model
                    , Cmd.batch
                        [ Api.deleteToken ()
                        , Nav.load "/login"
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        GotUpdatedAccount result ->
            ( model, Cmd.none )
