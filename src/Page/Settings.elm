module Page.Settings exposing (Model, Msg, init, update, view)

import Api
import Browser.Navigation as Nav
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, img, input, li, option, section, select, span, text, ul)
import Html.Attributes exposing (class, placeholder, selected, src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, string)
import Json.Encode as Encode
import MessageBanner as Message exposing (MessageBanner)
import Page.Loading
import Profile exposing (Config)
import RemoteData exposing (WebData)
import Request
import Route exposing (Token)
import Task
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
    , avatar : Maybe String
    }


type Msg
    = PickAvatar
    | FadeMessage
    | GotAvatar File
    | GotTimezones (WebData (List Timezone))
    | GotConnectToken (WebData String)
    | GotAvatarURL String
    | GotUpdatedProfile (Result Http.Error ())
    | GotUpdatedAccount (Result Http.Error ())
    | ChangeSubView View
    | StoreTimezone String
    | StoreName String
    | StoreEmail String
    | StoreCurrentPassword String
    | StoreNewPassword String
    | RequestConnectToken
    | UpdateProfile
    | UpdateAccount


type View
    = AccountSettings
    | ProfileSettings


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
            , avatar = Nothing
            }
    in
    ( model, fetchTimezones token )


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
        , expect = Http.expectWhatever GotUpdatedProfile
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


changeSettingsMenuClass : View -> View -> String
changeSettingsMenuClass subview selection =
    case subview == selection of
        True ->
            "active"

        False ->
            ""


settingsMenu : View -> Html Msg
settingsMenu subview =
    ul [ class "settings-menu" ]
        [ li
            [ changeSettingsMenuClass subview AccountSettings
                |> class
            , onClick (ChangeSubView AccountSettings)
            ]
            [ text "Account settings" ]
        , li
            [ changeSettingsMenuClass subview ProfileSettings
                |> class
            , onClick (ChangeSubView ProfileSettings)
            ]
            [ text "Profile settings" ]
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


profileSettingsView : Model -> Html Msg
profileSettingsView model =
    div [ class "profile-settings" ]
        [ div []
            [ span [] [ text "Name" ]
            , input [ onInput StoreName, placeholder "Enter your name", value model.name ] []
            , span [] [ text "Default Timezone" ]
            , viewTimezones ( model.timezones, model.timezone )
                |> select [ onInput StoreTimezone ]
            , span [] [ text "Connect Twitter" ]
            , button [ onClick RequestConnectToken ] [ text "Click to connect" ]
            , button [ onClick UpdateProfile ] [ text "Upate profile" ]
            ]
        , div [ class "avatar-settings-container" ]
            [ div [ class "avatar-container" ]
                [ img [ src (Page.Loading.getAvatarURL model.avatar), class "settings-avatar" ] []
                , span [ class "update-avatar", onClick PickAvatar ] [ text "Update avatar" ]
                ]
            ]
        ]


viewSettingsForms : Model -> Html Msg
viewSettingsForms model =
    case model.subView of
        AccountSettings ->
            accountSettingsView model

        ProfileSettings ->
            profileSettingsView model


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
        PickAvatar ->
            ( model
            , Select.file [ "image/*" ] GotAvatar
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

        GotAvatar file ->
            case File.size file > 2097152 of
                True ->
                    ( { model
                        | message = Just (Message.Failure "Please upload an image which is under 2MB in size")
                      }
                    , Message.fadeMessage FadeMessage
                    )

                False ->
                    ( model, Task.perform GotAvatarURL (File.toUrl file) )

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
            ( model, Message.fadeMessage FadeMessage )

        GotUpdatedAccount result ->
            ( model, Cmd.none )
