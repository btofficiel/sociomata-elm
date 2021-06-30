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
import Page.Loading exposing (header)
import Profile exposing (Config)
import RemoteData exposing (WebData)
import Request
import Route exposing (Route, Token)
import Task
import Timezone exposing (Timezone, timezonesDecoder)


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
    | RequestConnectToken Token
    | UpdateProfile Token
    | UpdateAccount Token


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


encodeNullable : (value -> Encode.Value) -> Maybe value -> Encode.Value
encodeNullable valueEncoder maybeValue =
    case maybeValue of
        Just value ->
            valueEncoder value

        Nothing ->
            Encode.null


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
        , ( "avatar", encodeNullable Encode.string (Api.base64FromUrl model.avatar) )
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


accountSettingsView : Model -> Token -> Html Msg
accountSettingsView model token =
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


profileSettingsView : Model -> Token -> Html Msg
profileSettingsView model token =
    div [ class "profile-settings" ]
        [ div []
            [ span [] [ text "Name" ]
            , input [ onInput StoreName, placeholder "Enter your name", value model.name ] []
            , span [] [ text "Default Timezone" ]
            , viewTimezones ( model.timezones, model.timezone )
                |> select [ onInput StoreTimezone ]
            , span [] [ text "Connect Twitter" ]
            , button [ onClick (RequestConnectToken token) ] [ text "Click to connect" ]
            , button [ onClick (UpdateProfile token) ] [ text "Upate profile" ]
            ]
        , div [ class "avatar-settings-container" ]
            [ div [ class "avatar-container" ]
                [ img [ src (Page.Loading.getAvatarURL model.avatar), class "settings-avatar" ] []
                , span [ class "update-avatar", onClick PickAvatar ] [ text "Update avatar" ]
                ]
            ]
        ]


viewSettingsForms : Model -> Token -> Html Msg
viewSettingsForms model token =
    case model.subView of
        AccountSettings ->
            accountSettingsView model token

        ProfileSettings ->
            profileSettingsView model token


body : Model -> Token -> Html Msg
body model token =
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
                            [ viewSettingsForms model token ]
                        ]
                    ]
                ]


viewMessage : MessageBanner -> Html Msg
viewMessage message =
    div
        [ class "message"
        , style "background"
            (case message of
                Just status ->
                    case status of
                        Message.Loading _ ->
                            "#ffff8b"

                        _ ->
                            "red"

                Nothing ->
                    "white"
            )
        , style "color"
            (case message of
                Just (Message.Loading msg) ->
                    "#0f0f0f"

                Just (Message.Failure msg) ->
                    "white"

                Just _ ->
                    "#0f0f0f"

                Nothing ->
                    "white"
            )
        ]
        [ span []
            [ text
                (case message of
                    Just (Message.Loading msg) ->
                        msg

                    Just (Message.Failure msg) ->
                        msg

                    Just _ ->
                        ""

                    Nothing ->
                        ""
                )
            ]
        ]


view : Model -> Token -> Route -> WebData Config -> Html Msg
view model token route config =
    div []
        [ header route (Profile.getAvatar config)
        , viewMessage model.message
        , body model token

        {---
        , button
            [ onClick PickAvatar ]
            [ text "Pick Avatar" ]
        , div [] [ text (Debug.toString model) ]
        , img [ src (avatarToString model.avatar), class "settings-avatar" ] []
        ---}
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PickAvatar ->
            ( model
            , Select.file [ "image/*" ] GotAvatar
            )

        FadeMessage ->
            ( { model | message = Nothing }, Cmd.none )

        GotConnectToken response ->
            case response of
                RemoteData.Success token ->
                    ( model, String.concat [ "/api/connect/twitter?token=", token ] |> Nav.load )

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

        RequestConnectToken token ->
            ( model, fetchConnectToken token )

        UpdateProfile token ->
            ( { model
                | message = Just (Message.Loading "Updating profile...")
              }
            , updateProfile model token
            )

        UpdateAccount token ->
            ( model, updateProfile model token )

        GotUpdatedProfile result ->
            ( model, Message.fadeMessage FadeMessage )

        GotUpdatedAccount result ->
            Debug.log (Debug.toString result)
                ( model, Cmd.none )
