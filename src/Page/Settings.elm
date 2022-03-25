port module Page.Settings exposing (Model, Msg(..), init, subscriptions, update, view)

import Api
import Browser.Navigation as Nav
import DateTime
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, hr, img, input, li, option, section, select, span, table, td, text, textarea, th, tr, ul)
import Html.Attributes exposing (class, id, placeholder, selected, src, style, type_, value)
import Html.Events exposing (onClick, onInput, onMouseOut, onMouseOver)
import Html.Keyed
import Http
import Json.Decode as Decode exposing (Decoder, string)
import Json.Encode as Encode
import MessageBanner as Message exposing (MessageBanner)
import Page.Loading
import Payments exposing (Transaction, transactionsDecoder)
import Plan exposing (Plan, plansDecoder)
import Profile exposing (Config, configDecoder)
import RemoteData exposing (WebData)
import Request
import Route exposing (Token)
import SocialAccount exposing (SocialAccount, socialAccountsDecoder)
import Task
import Team exposing (Member, inviteesEncoder, membersDecoder)
import Time
import Timezone exposing (Timezone, timezonesDecoder)
import Tweet as T


type Location
    = India
    | RoW


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
    , isAdmin : Bool
    , maxUsers : Int
    , maxAccounts : Int
    , members : WebData (List Member)
    , invitees : String
    , plans : WebData (List Plan)
    , socials : WebData (List SocialAccount)
    , yourPlan : Int
    , location : Location
    , toggledSocial : ToggledAccount
    }


type Msg
    = PickAvatar
    | FadeMessage
    | GotAvatar File
    | GotTimezones (WebData (List Timezone))
    | GotTransactions (WebData (List Transaction))
    | GotMembers (WebData (List Member))
    | GotPlans (WebData (List Plan))
    | GotSocialAccounts (WebData (List SocialAccount))
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
    | GotDeletedAccount Int (Result Http.Error ())
    | UpdateProfile
    | UpdateAccount
    | EnterInvitees String
    | InviteMembers
    | GotInvitees (Result Http.Error ())
    | ChangeCountry Location
    | MakePayment String
    | ToggleSocialActions Int
    | TurnOffSocialMenu
    | PromptDeleteSocial
    | DeleteSocial Int


type View
    = AccountSettings
    | ProfileSettings
    | PaymentSettings
    | TeamSettings
    | BillingPlans
    | SocialAccountsSettings


type alias ToggledAccount =
    { social : Maybe Int
    , askDeleteConfirmation : Bool
    }


init : Token -> ( Model, Cmd Msg )
init token =
    let
        model =
            { name = ""
            , email = ""
            , password = ""
            , newPassword = ""
            , timezone = 80
            , message = Just (Message.Onboarding "Facebook and Instagram integration are being tested in private beta and will be rolled out to everyone within 1-2 weeks")
            , subView = ProfileSettings
            , timezones = RemoteData.Loading
            , transactions = RemoteData.Loading
            , avatar = Nothing
            , isAdmin = False
            , maxUsers = 1
            , maxAccounts = 1
            , members = RemoteData.Loading
            , invitees = ""
            , plans = RemoteData.Loading
            , socials = RemoteData.Loading
            , yourPlan = 1
            , location = RoW
            , toggledSocial =
                { social = Nothing
                , askDeleteConfirmation = False
                }
            }
    in
    ( model, Cmd.batch [ fetchTimezones token, fetchTransactions token, fetchPlans token, fetchSocialAccounts token ] )


port switchPlan : String -> Cmd msg


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


inviteMembers : List String -> Token -> Cmd Msg
inviteMembers invitees token =
    let
        decoder =
            Decode.succeed ()
    in
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" token ]
        , url = "/api/invite"
        , body = Http.jsonBody (inviteesEncoder invitees)
        , expect = Request.expectJson GotInvitees decoder
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


fetchPlans : Token -> Cmd Msg
fetchPlans token =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" token ]
        , url = "/api/plans"
        , body = Http.emptyBody
        , expect = Request.expectJson (RemoteData.fromResult >> GotPlans) plansDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


fetchSocialAccounts : Token -> Cmd Msg
fetchSocialAccounts token =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" token ]
        , url = "/api/socials"
        , body = Http.emptyBody
        , expect = Request.expectJson (RemoteData.fromResult >> GotSocialAccounts) socialAccountsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


fetchMembers : Token -> Cmd Msg
fetchMembers token =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" token ]
        , url = "/api/members"
        , body = Http.emptyBody
        , expect = Request.expectJson (RemoteData.fromResult >> GotMembers) membersDecoder
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


disconnectSocial : Int -> Token -> Cmd Msg
disconnectSocial socialId token =
    let
        decoder =
            Decode.succeed ()

        url =
            "/api/connect/twitter/" ++ String.fromInt socialId
    in
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "Authorization" token ]
        , url = url
        , body = Http.emptyBody
        , expect = Request.expectJson (GotDeletedAccount socialId) decoder
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


settingsMenu : Model -> Html Msg
settingsMenu model =
    ul [ class "settings-menu" ]
        ([ {---li
            [ changeSettingsMenuClass  AccountSettings
                |> class
            , onClick (ChangeSubView AccountSettings)
            ]
            [ text "Account settings" ]
            ,--}
           li
            [ changeSettingsMenuClass model.subView ProfileSettings
                |> class
            , onClick (ChangeSubView ProfileSettings)
            ]
            [ text "Profile settings" ]
         , li
            [ changeSettingsMenuClass model.subView SocialAccountsSettings
                |> class
            , onClick (ChangeSubView SocialAccountsSettings)
            ]
            [ text "Social Accounts" ]
         ]
            ++ (if model.isAdmin == True && model.maxUsers > 1 then
                    [ li
                        [ changeSettingsMenuClass model.subView TeamSettings
                            |> class
                        , onClick (ChangeSubView TeamSettings)
                        ]
                        [ text "Team settings" ]
                    ]

                else
                    []
               )
            ++ (if model.isAdmin == True then
                    [ li
                        [ changeSettingsMenuClass model.subView PaymentSettings
                            |> class
                        , onClick (ChangeSubView PaymentSettings)
                        ]
                        [ text "My Transactions" ]
                    , li
                        [ changeSettingsMenuClass model.subView BillingPlans
                            |> class
                        , onClick (ChangeSubView BillingPlans)
                        ]
                        [ text "Billing Plans" ]
                    ]

                else
                    []
               )
        )


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



{--

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
            --}


profileSettingsView : Model -> Html Msg
profileSettingsView model =
    div [ class "profile-settings" ]
        [ div []
            [ span [] [ text "Name" ]
            , input [ onInput StoreName, placeholder "Enter your name", value model.name ] []
            , span [] [ text "Default Timezone" ]
            , viewTimezones ( model.timezones, model.timezone )
                |> select [ onInput StoreTimezone ]
            , button [ onClick UpdateProfile ] [ text "Update profile" ]
            ]
        , div [ class "avatar-settings-container" ]
            [ div [ class "avatar-container" ]
                [ img [ src (Page.Loading.getAvatarURL model.avatar), class "settings-avatar" ] []
                , span [ class "update-avatar", onClick PickAvatar ] [ text "Update avatar" ]
                ]
            ]
        ]


viewMember : Member -> Html Msg
viewMember member =
    let
        avatar =
            Page.Loading.getAvatarURL member.avatar

        name =
            case member.name of
                Just name_ ->
                    name_

                Nothing ->
                    "Team Member"
    in
    li [ class "plug" ]
        [ img [ class "avatar", src avatar ] []
        , span [ class "plug-name" ] [ text name ]
        , span [ class "content" ] [ text member.email ]
        ]


viewTeam : List Member -> Html Msg
viewTeam members =
    div [ class "day" ]
        [ ul []
            (List.map viewMember members)
        ]


inviteMember : String -> Html Msg
inviteMember invitees =
    div []
        [ div [ class "editor" ]
            [ textarea [ placeholder "Enter invitee email by entering a comma between them", value invitees, onInput EnterInvitees ] []
            ]
        , span [ class "button" ] [ button [ onClick InviteMembers ] [ text "Invite" ] ]
        ]


teamSettingsView : Model -> Html Msg
teamSettingsView model =
    case model.members of
        RemoteData.Success members ->
            div [ class "team-settings" ]
                [ div [ class "heading add-account" ]
                    [ span []
                        [ text "Team members" ]
                    , span [ class "twitter-counter" ]
                        [ span []
                            [ text
                                (String.concat
                                    [ String.fromInt (List.length members)
                                    , "/"
                                    , String.fromInt model.maxUsers
                                    ]
                                )
                            ]
                        , span [ class "tweet-count-indicator" ]
                            [ span
                                [ class "tweet-count-progress"
                                , style "width"
                                    (String.concat
                                        [ calculateAccountLimit (List.length members) (toFloat model.maxUsers)
                                        , "rem"
                                        ]
                                    )
                                ]
                                []
                            ]
                        ]
                    ]
                , div [ class "heading" ] [ text "Invite members" ]
                , viewTeam members
                , inviteMember model.invitees
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


viewSocialAccount : ToggledAccount -> SocialAccount -> Html Msg
viewSocialAccount toggledSocial social =
    let
        socialIdString =
            social.id
                |> String.fromInt

        settingsIconId =
            socialIdString
                |> (++) "settingsIcon"

        socialMenuId =
            socialIdString
                |> (++) "postMenu"
    in
    li [ class "plug" ]
        [ img [ class "avatar", src "/images/twitter.png" ] []
        , span [ class "plug-name" ] [ text social.description ]
        , span [ class "content" ] [ text "Twitter" ]
        , img
            [ id settingsIconId
            , class "settings-icon"
            , src "/images/settings.svg"
            , onClick (ToggleSocialActions social.id)
            ]
            []
        , span
            [ id socialMenuId
            , class "post-menu"
            , style "display"
                (case toggledSocial.social of
                    Just socialId ->
                        case socialId == social.id of
                            True ->
                                "block"

                            False ->
                                "none"

                    Nothing ->
                        "none"
                )
            ]
            (case toggledSocial.askDeleteConfirmation of
                True ->
                    [ div [] [ text "Delete this social account?" ]
                    , ul [ class "settings-menu" ]
                        [ li [ onClick (DeleteSocial social.id) ] [ text "Yes" ]
                        , li [ onClick TurnOffSocialMenu ] [ text "No" ]
                        ]
                    ]

                False ->
                    [ ul [ class "settings-menu" ]
                        [ li [ onClick PromptDeleteSocial ] [ text "Delete" ]
                        ]
                    ]
            )
        ]


socialAccountSettings : Model -> Html Msg
socialAccountSettings model =
    case model.socials of
        RemoteData.Success socials ->
            div [ class "social-account-settings" ]
                [ div [ class "add-account" ]
                    [ span [ class "heading" ]
                        [ text "Connect a new account" ]
                    ]
                , div [ class "platforms" ]
                    [ div [ class "platform" ]
                        [ img [ class "social-logo", src "/images/twitter.png" ] []
                        , span [ class "platform-name" ] [ text "Twitter" ]
                        , span [ class "platform-account" ] [ text "Profile" ]
                        , span [ class "button" ]
                            [ button [ onClick RequestConnectToken ] [ text "Connect" ]
                            ]
                        ]
                    , div [ class "platform" ]
                        [ img [ class "social-logo", src "/images/fb.png" ] []
                        , span [ class "platform-name" ] [ text "Facebook" ]
                        , span [ class "platform-account" ] [ text "Page or Group" ]
                        , span [ class "button soon" ]
                            [ button [] [ text "Soon" ]
                            ]
                        ]
                    , div [ class "platform" ]
                        [ img [ class "social-logo", src "/images/ig.png" ] []
                        , span [ class "platform-name" ] [ text "Instagram" ]
                        , span [ class "platform-account" ] [ text "Business account" ]
                        , span [ class "button soon" ]
                            [ button [] [ text "Soon" ]
                            ]
                        ]
                    , div [ class "platform" ]
                        [ img [ class "social-logo", src "/images/linkedin.png" ] []
                        , span [ class "platform-name" ] [ text "LinkedIn" ]
                        , span [ class "platform-account" ] [ text "Page or Profile" ]
                        , span [ class "button soon" ]
                            [ button [] [ text "Soon" ]
                            ]
                        ]
                    ]
                , div [ class "add-account" ]
                    [ span [ class "heading" ]
                        [ text "Connected accounts" ]
                    , span [ class "twitter-counter" ]
                        [ span []
                            [ text
                                (String.concat
                                    [ String.fromInt (List.length socials)
                                    , "/"
                                    , String.fromInt model.maxAccounts
                                    ]
                                )
                            ]
                        , span [ class "tweet-count-indicator" ]
                            [ span
                                [ class "tweet-count-progress"
                                , style "width"
                                    (String.concat
                                        [ calculateAccountLimit (List.length socials) (toFloat model.maxAccounts)
                                        , "rem"
                                        ]
                                    )
                                ]
                                []
                            ]
                        ]
                    ]
                , List.map (\s -> ( String.fromInt s.id, viewSocialAccount model.toggledSocial s )) socials
                    |> Html.Keyed.ul []
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


viewPlan : Int -> Location -> Plan -> Html Msg
viewPlan currentPlan location plan =
    let
        currency =
            case location of
                India ->
                    "₹"

                RoW ->
                    "$"

        price =
            String.concat [ currency, String.fromInt plan.price, "/year" ]
    in
    li [ class "billing-plan" ]
        [ span [] [ text plan.name ]
        , span [] [ text price ]
        , case currentPlan == plan.id of
            True ->
                span [ class "active" ] [ text "Active" ]

            False ->
                span [ class "button" ]
                    [ button [ onClick (MakePayment plan.priceId) ] [ text "Make payment" ]
                    ]
        ]


billingPlans : Model -> Html Msg
billingPlans model =
    case model.plans of
        RemoteData.Success plans ->
            let
                plans_ =
                    case model.location of
                        India ->
                            plans
                                |> List.filter (\p -> p.symbol == "inr")

                        RoW ->
                            plans
                                |> List.filter (\p -> p.symbol == "usd")
            in
            div [ class "billing-settings" ]
                [ div [ class "heading" ] [ text "Billing Plans" ]
                , div [ class "heading" ] [ text "Select your location" ]
                , ul []
                    (List.map (viewPlan model.yourPlan model.location) plans_)
                , div []
                    [ div [ class "country-selection" ]
                        [ div
                            [ class
                                (case model.location of
                                    India ->
                                        "country active"

                                    RoW ->
                                        "country"
                                )
                            , onClick (ChangeCountry India)
                            ]
                            [ text "🇮🇳 India" ]
                        , div
                            [ class
                                (case model.location of
                                    India ->
                                        "country"

                                    RoW ->
                                        "country active"
                                )
                            , onClick (ChangeCountry RoW)
                            ]
                            [ text "🌎 Other" ]
                        ]
                    ]
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


viewSettingsForms : Model -> Html Msg
viewSettingsForms model =
    case model.subView of
        AccountSettings ->
            accountSettingsView model

        ProfileSettings ->
            profileSettingsView model

        PaymentSettings ->
            paymentSettingsView model

        TeamSettings ->
            teamSettingsView model

        SocialAccountsSettings ->
            socialAccountSettings model

        BillingPlans ->
            billingPlans model


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
                                [ settingsMenu model
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


port setToggledSocialIds : Int -> Cmd msg


port turnOffSocialMenu : (() -> msg) -> Sub msg


subscriptions : Sub Msg
subscriptions =
    turnOffSocialMenu (always TurnOffSocialMenu)


update : Msg -> Model -> Token -> ( Model, Cmd Msg )
update msg model token =
    case msg of
        TurnOffSocialMenu ->
            ( { model
                | toggledSocial =
                    { social = Nothing
                    , askDeleteConfirmation = False
                    }
              }
            , Cmd.none
            )

        PromptDeleteSocial ->
            let
                currentToggledSocial =
                    model.toggledSocial

                new_toggleSocial =
                    { currentToggledSocial
                        | askDeleteConfirmation = True
                    }
            in
            ( { model
                | toggledSocial = new_toggleSocial
              }
            , Cmd.none
            )

        ToggleSocialActions socialId ->
            case model.toggledSocial.social of
                Just pid ->
                    case pid == socialId of
                        True ->
                            ( { model
                                | toggledSocial =
                                    { social = Nothing
                                    , askDeleteConfirmation = False
                                    }
                              }
                            , Cmd.none
                            )

                        False ->
                            ( { model
                                | toggledSocial =
                                    { social = Just socialId
                                    , askDeleteConfirmation = False
                                    }
                              }
                            , setToggledSocialIds socialId
                            )

                Nothing ->
                    ( { model
                        | toggledSocial =
                            { social = Just socialId
                            , askDeleteConfirmation = False
                            }
                      }
                    , setToggledSocialIds socialId
                    )

        MakePayment priceId ->
            ( model, switchPlan priceId )

        ChangeCountry country ->
            ( { model | location = country }, Cmd.none )

        EnterInvitees invitees ->
            ( { model | invitees = invitees }, Cmd.none )

        InviteMembers ->
            let
                payload =
                    model.invitees
                        |> String.split ","
                        |> List.map String.toLower
                        |> List.map String.trim
            in
            ( { model | message = Just Message.Loading }, inviteMembers payload token )

        DeleteSocial socialId ->
            ( { model
                | message = Just Message.Loading
              }
            , disconnectSocial socialId token
            )

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

        GotMembers response ->
            case response of
                RemoteData.Success _ ->
                    ( { model | members = response }, Cmd.none )

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
                            ( { model | members = response }, Cmd.none )

                _ ->
                    ( { model | members = response }, Cmd.none )

        GotSocialAccounts response ->
            case response of
                RemoteData.Success _ ->
                    ( { model | socials = response }, Cmd.none )

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
                            ( { model | socials = response }, Cmd.none )

                _ ->
                    ( { model | socials = response }, Cmd.none )

        GotPlans response ->
            case response of
                RemoteData.Success _ ->
                    ( { model | plans = response }, Cmd.none )

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
                            ( { model | plans = response }, Cmd.none )

                _ ->
                    ( { model | plans = response }, Cmd.none )

        GotDeletedAccount socialId (Ok _) ->
            case model.socials of
                RemoteData.Success socials ->
                    let
                        new_socials =
                            socials
                                |> List.filter (\p -> (p.id == socialId) == False)
                    in
                    ( { model
                        | message = Just (Message.Success "Your twitter account has been disconnected")
                        , socials = RemoteData.Success new_socials
                      }
                    , Message.fadeMessage FadeMessage
                    )

                _ ->
                    ( model, Nav.reload )

        GotDeletedAccount _ (Err error) ->
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
            case subview of
                TeamSettings ->
                    ( { model | subView = subview }, fetchMembers token )

                _ ->
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

        GotInvitees result ->
            case result of
                Ok _ ->
                    let
                        message =
                            "Yes! Invitation mails have been sent"
                    in
                    ( { model
                        | message = Just (Message.Success message)
                      }
                    , Message.fadeMessage FadeMessage
                    )

                Err (Http.BadBody err) ->
                    ( { model | message = Just (Message.Failure err) }, Message.fadeMessage FadeMessage )

                Err Http.NetworkError ->
                    let
                        err =
                            "Oops! Looks like there is some problem with your network."
                    in
                    ( { model | message = Just (Message.Failure err) }, Message.fadeMessage FadeMessage )

                Err (Http.BadStatus 401) ->
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
