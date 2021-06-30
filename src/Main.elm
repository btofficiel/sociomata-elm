module Main exposing (main)

import Api
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Page.Loading as Loading
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
    }


type AuthenticationStatus
    = Unknown Route
    | LoggedIn Token
    | LoggedOut


type Page
    = NotFoundPage
    | LoadingPage
    | QueuePage Queue.Model
    | SettingsPage Settings.Model


type Msg
    = LinkClicked UrlRequest
    | QueueMsg Queue.Msg
    | SettingsMsg Settings.Msg
    | GotConfig Route Token (WebData Config)
    | UrlChanged Url


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
            }
    in
    initCurrentPage ( model, fetchConfig (Maybe.withDefault "invalid_token" flags) (parseRoute model.auth) )


initCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPage ( model, existingCmds ) =
    let
        ( currentPage, mappedPageCmds ) =
            case model.route of
                Route.Loading ->
                    ( LoadingPage, Cmd.none )

                Route.NotFound ->
                    ( LoadingPage, Cmd.none )

                Route.Index ->
                    let
                        ( pageModel, pageCmds ) =
                            Queue.init (getToken model.auth)
                    in
                    ( QueuePage pageModel, Cmd.map QueueMsg pageCmds )

                Route.CreatePostQuery query ->
                    ( LoadingPage, Cmd.none )

                Route.Settings ->
                    let
                        ( pageModel, pageCmds ) =
                            Settings.init (getToken model.auth)

                        ( updatedPageModel, updatePageCmds ) =
                            case model.config of
                                RemoteData.Success config ->
                                    case config.profile of
                                        Just profile ->
                                            Debug.log (Debug.toString profile)
                                                ( { pageModel
                                                    | name = profile.name
                                                    , timezone = profile.timezoneId
                                                    , avatar = profile.avatar
                                                    , email = config.email
                                                  }
                                                , pageCmds
                                                )

                                        Nothing ->
                                            ( { pageModel
                                                | email = config.email
                                              }
                                            , pageCmds
                                            )

                                RemoteData.Loading ->
                                    Debug.log "Here!!"
                                        ( pageModel, pageCmds )

                                _ ->
                                    Debug.log "Here"
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

        Route.CreatePostQuery query ->
            "Create Post - Sociomata"

        Route.Index ->
            "Posts - Sociomata"

        Route.Settings ->
            "Account Settings - Sociomata"

        Route.NotFound ->
            "Not Found"


currentView : Model -> Html Msg
currentView model =
    case model.page of
        LoadingPage ->
            Loading.loadingView model.route model.config

        NotFoundPage ->
            Loading.loadingView model.route model.config

        QueuePage pageModel ->
            Queue.view pageModel (getToken model.auth) model.route model.config
                |> Html.map QueueMsg

        SettingsPage pageModel ->
            Settings.view pageModel (getToken model.auth) model.route model.config
                |> Html.map SettingsMsg


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

                _ ->
                    initCurrentPage
                        ( { model
                            | route = route
                            , config = config
                            , auth = LoggedIn token
                          }
                        , Cmd.none
                        )

        ( LinkClicked _, _ ) ->
            ( model, Cmd.none )

        ( UrlChanged _, _ ) ->
            ( model, Cmd.none )

        ( QueueMsg subMsg, QueuePage pageModel ) ->
            let
                ( updatedPageModel, updateCmds ) =
                    Queue.update subMsg pageModel (Profile.getOffset model.config)
            in
            ( { model | page = QueuePage updatedPageModel }, Cmd.map QueueMsg updateCmds )

        ( SettingsMsg subMsg, SettingsPage pageModel ) ->
            let
                ( updatedPageModel, updateCmds ) =
                    Settings.update subMsg pageModel
            in
            ( { model | page = SettingsPage updatedPageModel }, Cmd.map SettingsMsg updateCmds )

        ( _, _ ) ->
            ( model, Cmd.none )


main : Program (Maybe Token) Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
