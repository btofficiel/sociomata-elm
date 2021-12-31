port module Main exposing (main)

import Api
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Page.CreatePost as CreatePost
import Page.EditPost as EditPost
import Page.Loading as Loading
import Page.Onboarding as Onboarding
import Page.Queue as Queue
import Page.Settings as Settings
import Process
import Profile exposing (Config, configDecoder)
import RemoteData exposing (WebData)
import Request
import Route exposing (Query, Route, Token, parseUrl)
import Task
import Time
import Url exposing (Url)


type alias Model =
    { route : Route
    , page : Page
    , config : WebData Config
    , auth : AuthenticationStatus
    , navKey : Nav.Key
    , toggleMenu : Bool
    }


type AuthenticationStatus
    = Unknown Route
    | LoggedIn Token
    | LoggedOut


type Page
    = NotFoundPage
    | LoadingPage
    | PaymentSuccessPage
    | PaymentFailedPage
    | OnboardingPage Onboarding.Model
    | QueuePage Queue.Model
    | CreatePostPage CreatePost.Model
    | EditPostPage EditPost.Model
    | SettingsPage Settings.Model


type Msg
    = LinkClicked UrlRequest
    | Redirect String
    | QueueMsg Queue.Msg
    | OnboardingMsg Onboarding.Msg
    | CreatePostMsg CreatePost.Msg
    | EditPostMsg EditPost.Msg
    | SettingsMsg Settings.Msg
    | GotConfig Route Token (WebData Config)
    | UrlChanged Url
    | ToggleMenu
    | TurnOffMenu
    | Logout


getToken : AuthenticationStatus -> Token
getToken auth =
    case auth of
        LoggedIn token ->
            token

        _ ->
            "invalid_token"


parseRoute : AuthenticationStatus -> Route
parseRoute auth =
    case auth of
        Unknown route ->
            route

        LoggedIn _ ->
            Route.Index

        LoggedOut ->
            Route.Index


fetchConfig : Token -> Route -> Cmd Msg
fetchConfig token route =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" token ]
        , url = "/api/profile"
        , body = Http.emptyBody
        , expect = Request.expectJson (RemoteData.fromResult >> GotConfig route token) configDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


init : Maybe Token -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        model =
            { route = Route.Loading
            , page = LoadingPage
            , config = RemoteData.Loading
            , auth = Unknown (parseUrl url)
            , navKey = navKey
            , toggleMenu = False
            }
    in
    initCurrentPage
        ( model
        , fetchConfig (Maybe.withDefault "invalid_token" flags) (parseRoute model.auth)
        )


initCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPage ( model, existingCmds ) =
    let
        ( currentPage, mappedPageCmds ) =
            case model.route of
                Route.Loading ->
                    ( LoadingPage, Cmd.none )

                Route.NotFound ->
                    ( LoadingPage, Cmd.none )

                Route.PaymentSucceeded ->
                    ( PaymentSuccessPage
                    , Process.sleep 5000
                        |> Task.perform (\_ -> Redirect "/app")
                    )

                Route.PaymentFailed ->
                    ( PaymentFailedPage
                    , Process.sleep 5000
                        |> Task.perform (\_ -> Redirect "/app/onboarding")
                    )

                Route.Onboarding ->
                    let
                        ( pageModel, pageCmds ) =
                            Onboarding.init
                    in
                    ( OnboardingPage pageModel, Cmd.map OnboardingMsg pageCmds )

                Route.Index ->
                    let
                        ( pageModel, pageCmds ) =
                            Queue.init (getToken model.auth)
                    in
                    ( QueuePage pageModel, Cmd.map QueueMsg pageCmds )

                Route.EditPost postId ->
                    let
                        ( pageModel, pageCmds ) =
                            EditPost.init postId (getToken model.auth)
                    in
                    ( EditPostPage pageModel, Cmd.map EditPostMsg pageCmds )

                Route.CreatePost query ->
                    let
                        ( pageModel, pageCmds ) =
                            CreatePost.init query
                    in
                    ( CreatePostPage pageModel, Cmd.map CreatePostMsg pageCmds )

                Route.Settings ->
                    let
                        ( pageModel, pageCmds ) =
                            Settings.init (getToken model.auth)

                        ( updatedPageModel, updatePageCmds ) =
                            case model.config of
                                RemoteData.Success config ->
                                    let
                                        twitter =
                                            { connected = config.twitterConnected
                                            , showDisconnectButon = False
                                            }
                                    in
                                    case config.profile of
                                        Just profile ->
                                            ( { pageModel
                                                | name = profile.name
                                                , timezone = profile.timezoneId
                                                , avatar = profile.avatar
                                                , email = config.email
                                                , twitter = twitter
                                              }
                                            , pageCmds
                                            )

                                        Nothing ->
                                            ( { pageModel
                                                | email = config.email
                                                , twitter = twitter
                                              }
                                            , pageCmds
                                            )

                                RemoteData.Loading ->
                                    ( pageModel, pageCmds )

                                _ ->
                                    ( pageModel, pageCmds )
                    in
                    ( SettingsPage updatedPageModel, Cmd.map SettingsMsg updatePageCmds )
    in
    ( { model | page = currentPage }
    , Cmd.batch [ existingCmds, mappedPageCmds ]
    )



{-
   delay : Float -> Model -> Maybe Token -> Cmd Msg
   delay time model token =
       Process.sleep time
           |> Task.perform (\_ -> GotAuth (parseRoute model.auth) token)
-}


currentTitle : Route -> String
currentTitle route =
    case route of
        Route.Loading ->
            "Loading..."

        Route.PaymentSucceeded ->
            "Payment successful..."

        Route.PaymentFailed ->
            "Payment failed"

        Route.Onboarding ->
            "Select a plan - Sociomata"

        Route.CreatePost query ->
            "Create Post - Sociomata"

        Route.EditPost postId ->
            "Edit Post - Sociomata"

        Route.Index ->
            "Posts - Sociomata"

        Route.Settings ->
            "Account Settings - Sociomata"

        Route.NotFound ->
            "Not Found"


currentView : Model -> Html Msg
currentView model =
    div []
        [ Loading.header model.route (Profile.getAvatar model.config) model.toggleMenu ( ToggleMenu, Logout )
        , case model.page of
            LoadingPage ->
                Loading.loadingView model.config

            PaymentSuccessPage ->
                "Your payment has been successful, we're taking you back to the app"
                    |> Loading.redirection 80

            PaymentFailedPage ->
                "Your payment failed, we're taking you back to the payment page"
                    |> Loading.redirection 80

            NotFoundPage ->
                Loading.emptyState 80 "Page not found"

            OnboardingPage pageModel ->
                Onboarding.view pageModel
                    |> Html.map OnboardingMsg

            EditPostPage pageModel ->
                EditPost.view pageModel model.config
                    |> Html.map EditPostMsg

            CreatePostPage pageModel ->
                CreatePost.view pageModel model.config
                    |> Html.map CreatePostMsg

            QueuePage pageModel ->
                Queue.view pageModel model.config
                    |> Html.map QueueMsg

            SettingsPage pageModel ->
                Settings.view pageModel model.config
                    |> Html.map SettingsMsg
        ]


view : Model -> Document Msg
view model =
    { title = currentTitle model.route
    , body = [ currentView model ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( GotConfig route token config, _ ) ->
            case config of
                RemoteData.Failure (Http.BadStatus 401) ->
                    ( model
                    , Cmd.batch
                        [ Api.deleteToken ()
                        , Nav.load "/login"
                        ]
                    )

                RemoteData.Success conf ->
                    case conf.account.trialActive || conf.account.subActive of
                        True ->
                            initCurrentPage
                                ( { model
                                    | route = route
                                    , config = config
                                    , auth = LoggedIn token
                                  }
                                , Cmd.none
                                )

                        False ->
                            case route of
                                Route.PaymentSucceeded ->
                                    initCurrentPage
                                        ( { model
                                            | route = route
                                            , config = config
                                            , auth = LoggedIn token
                                          }
                                        , Cmd.none
                                        )

                                Route.PaymentFailed ->
                                    initCurrentPage
                                        ( { model
                                            | route = route
                                            , config = config
                                            , auth = LoggedIn token
                                          }
                                        , Cmd.none
                                        )

                                _ ->
                                    ( { model
                                        | config = config
                                        , auth = LoggedIn token
                                      }
                                    , Nav.replaceUrl model.navKey "/app/onboarding"
                                    )

                _ ->
                    initCurrentPage
                        ( { model
                            | route = route
                            , config = config
                            , auth = LoggedIn token
                          }
                        , Cmd.none
                        )

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.navKey (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

        ( ToggleMenu, _ ) ->
            let
                toggle =
                    case model.toggleMenu of
                        True ->
                            False

                        False ->
                            True
            in
            ( { model | toggleMenu = toggle }, Cmd.none )

        ( TurnOffMenu, _ ) ->
            ( { model | toggleMenu = False }, Cmd.none )

        ( Logout, _ ) ->
            ( model
            , Cmd.batch
                [ Api.deleteToken ()
                , Nav.load "/login"
                ]
            )

        ( Redirect url, _ ) ->
            ( model, Nav.replaceUrl model.navKey url )

        ( UrlChanged url, _ ) ->
            let
                newRoute =
                    Route.parseUrl url
            in
            ( { model | route = newRoute }, Cmd.none )
                |> initCurrentPage

        ( EditPostMsg subMsg, EditPostPage pageModel ) ->
            let
                ( updatedPageModel, updateCmds ) =
                    EditPost.update subMsg pageModel (Profile.getOffset model.config) (getToken model.auth)
            in
            ( { model | page = EditPostPage updatedPageModel }, Cmd.map EditPostMsg updateCmds )

        ( OnboardingMsg subMsg, OnboardingPage pageModel ) ->
            let
                ( updatedPageModel, updateCmds ) =
                    Onboarding.update subMsg pageModel
            in
            ( { model | page = OnboardingPage updatedPageModel }, Cmd.map OnboardingMsg updateCmds )

        ( CreatePostMsg subMsg, CreatePostPage pageModel ) ->
            let
                ( updatedPageModel, updateCmds ) =
                    CreatePost.update subMsg pageModel (Profile.getOffset model.config) (getToken model.auth)
            in
            ( { model | page = CreatePostPage updatedPageModel }, Cmd.map CreatePostMsg updateCmds )

        ( QueueMsg subMsg, QueuePage pageModel ) ->
            let
                ( updatedPageModel, updateCmds ) =
                    Queue.update subMsg pageModel (Profile.getOffset model.config) (getToken model.auth) model.navKey
            in
            ( { model | page = QueuePage updatedPageModel }, Cmd.map QueueMsg updateCmds )

        ( SettingsMsg subMsg, SettingsPage pageModel ) ->
            let
                ( updatedPageModel, updateCmds ) =
                    Settings.update subMsg pageModel (getToken model.auth)
            in
            case subMsg of
                Settings.GotUpdatedProfile response ->
                    case response of
                        RemoteData.Success config ->
                            ( { model
                                | page = SettingsPage updatedPageModel
                                , config = response
                              }
                            , Cmd.map SettingsMsg updateCmds
                            )

                        _ ->
                            ( { model | page = SettingsPage updatedPageModel }, Cmd.map SettingsMsg updateCmds )

                Settings.GotDeletedAccount (Ok _) ->
                    case model.config of
                        RemoteData.Success config ->
                            let
                                new_config =
                                    { config
                                        | twitterConnected = False
                                    }
                            in
                            ( { model
                                | page = SettingsPage updatedPageModel
                                , config = RemoteData.Success new_config
                              }
                            , Cmd.map SettingsMsg updateCmds
                            )

                        _ ->
                            ( { model | page = SettingsPage updatedPageModel }, Cmd.map SettingsMsg updateCmds )

                _ ->
                    ( { model | page = SettingsPage updatedPageModel }, Cmd.map SettingsMsg updateCmds )

        ( _, _ ) ->
            ( model, Cmd.none )


port turnOffMenu : (() -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ turnOffMenu (always TurnOffMenu)
        , case model.page of
            QueuePage _ ->
                Queue.subscriptions
                    |> Sub.map QueueMsg

            _ ->
                Sub.none
        ]


main : Program (Maybe Token) Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
