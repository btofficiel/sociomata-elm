port module Page.Plugs exposing (Model, Msg, init, subscriptions, update, view)

import Api
import Browser.Navigation as Nav
import Html exposing (Attribute, Html, a, div, img, li, section, span, text, ul)
import Html.Attributes exposing (class, href, id, src, style)
import Html.Events exposing (onClick)
import Html.Keyed
import Http
import Json.Decode as D
import MessageBanner as Message exposing (MessageBanner)
import Page.Loading
import Plug exposing (Plug, plugsDecoder)
import Profile exposing (Avatar, Config)
import RemoteData exposing (WebData)
import Request
import Route exposing (Token)


type Msg
    = FadeMessage
    | OpenCreatePlug (Maybe Int)
    | OpenEditPlug Int
    | GotPlugs (WebData (List Plug))
    | TogglePlugActions Int
    | TurnOffPlugMenu
    | PromptDeletePlug
    | DeletePlug Int
    | GotDeletedPlug Int (Result Http.Error ())


type alias ToggledPlug =
    { plug : Maybe Int
    , askDeleteConfirmation : Bool
    }


type alias Model =
    { plugs : WebData (List Plug)
    , toggledPlug : ToggledPlug
    , message : MessageBanner
    }


deletePlug : Token -> Int -> Cmd Msg
deletePlug token plugId =
    let
        decoder =
            D.succeed ()

        url =
            String.concat [ "/api/plugs/", String.fromInt plugId ]
    in
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "Authorization" token ]
        , url = url
        , body = Http.emptyBody
        , expect = Request.expectJson (GotDeletedPlug plugId) decoder
        , timeout = Nothing
        , tracker = Nothing
        }


fetchPlugs : Token -> Cmd Msg
fetchPlugs token =
    let
        url =
            "/api/plugs"
    in
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" token ]
        , url = url
        , body = Http.emptyBody
        , expect = Request.expectJson (RemoteData.fromResult >> GotPlugs) plugsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


init : Token -> ( Model, Cmd Msg )
init token =
    ( { plugs = RemoteData.Loading
      , toggledPlug =
            { plug = Nothing
            , askDeleteConfirmation = False
            }
      , message = Nothing
      }
    , fetchPlugs token
    )


viewPlug : ToggledPlug -> Avatar -> Plug -> Html Msg
viewPlug toggledPlug avatar plug =
    let
        plugIdString =
            plug.id
                |> String.fromInt

        settingsIconId =
            plugIdString
                |> (++) "settingsIcon"

        plugMenuId =
            plugIdString
                |> (++) "postMenu"

        content =
            plug.content
                |> String.slice 0 100
    in
    li [ class "plug" ]
        [ img [ class "avatar", src (Page.Loading.getAvatarURL avatar) ] []
        , span [ class "plug-name" ] [ text plug.name ]
        , span [ class "content", onClick (OpenEditPlug plug.id) ] [ text content ]
        , img
            [ id settingsIconId
            , class "settings-icon"
            , src "/images/settings.svg"
            , onClick (TogglePlugActions plug.id)
            ]
            []
        , span
            [ id plugMenuId
            , class "post-menu"
            , style "display"
                (case toggledPlug.plug of
                    Just plugId ->
                        case plugId == plug.id of
                            True ->
                                "block"

                            False ->
                                "none"

                    Nothing ->
                        "none"
                )
            ]
            (case toggledPlug.askDeleteConfirmation of
                True ->
                    [ div [] [ text "Delete this plug?" ]
                    , ul [ class "settings-menu" ]
                        [ li [ onClick (DeletePlug plug.id) ] [ text "Yes" ]
                        , li [ onClick TurnOffPlugMenu ] [ text "No" ]
                        ]
                    ]

                False ->
                    [ ul [ class "settings-menu" ]
                        [ li [ onClick (OpenEditPlug plug.id) ] [ text "Edit plug" ]
                        , li [ onClick PromptDeletePlug ] [ text "Delete plug" ]
                        ]
                    ]
            )
        ]


body : Model -> WebData Config -> Html Msg
body model config =
    let
        avatar =
            Profile.getAvatar config
    in
    case model.plugs of
        RemoteData.NotAsked ->
            Page.Loading.body

        RemoteData.Loading ->
            Page.Loading.body

        RemoteData.Success plugs ->
            case List.length plugs > 0 of
                True ->
                    div [ class "container" ]
                        [ section [ class "panels" ]
                            [ section [ class "subheader" ]
                                [ span [] [ text "Plugs" ]
                                , span [ onClick (OpenCreatePlug Nothing) ] [ text "Create Plug" ]
                                ]
                            , section [ class "one-panel" ]
                                [ div [ class "queue" ]
                                    [ div [ class "day" ]
                                        [ List.map
                                            (\plug ->
                                                ( plug.id
                                                    |> String.fromInt
                                                , viewPlug model.toggledPlug avatar plug
                                                )
                                            )
                                            plugs
                                            |> Html.Keyed.ul []
                                        ]
                                    ]
                                ]
                            ]
                        ]

                False ->
                    div [ class "container" ]
                        [ section [ class "panels" ]
                            [ Page.Loading.emptyState 80 "You haven't saved any plugs yet"
                            ]
                        ]

        RemoteData.Failure (Http.BadBody err) ->
            div [ class "container" ]
                [ section [ class "panels" ]
                    [ Page.Loading.emptyState 80 err
                    ]
                ]

        RemoteData.Failure Http.NetworkError ->
            div [ class "container" ]
                [ section [ class "panels" ]
                    [ Page.Loading.emptyState 80 "Oops! Looks like there is some problem with your network."
                    ]
                ]

        _ ->
            div [] []


view : Model -> WebData Config -> Html Msg
view model config =
    div []
        [ Message.viewMessage model.message
        , body model config
        ]


port setToggledPlugIds : Int -> Cmd msg


port turnOffPlugMenu : (() -> msg) -> Sub msg


subscriptions : Sub Msg
subscriptions =
    turnOffPlugMenu (always TurnOffPlugMenu)


update : Msg -> Model -> Token -> Nav.Key -> ( Model, Cmd Msg )
update msg model token navKey =
    case msg of
        TurnOffPlugMenu ->
            ( { model
                | toggledPlug =
                    { plug = Nothing
                    , askDeleteConfirmation = False
                    }
              }
            , Cmd.none
            )

        OpenEditPlug plugId ->
            ( model, Nav.pushUrl navKey (String.concat [ "/app/plugs/", String.fromInt plugId ]) )

        PromptDeletePlug ->
            let
                currentToggledPlug =
                    model.toggledPlug

                new_togglePlug =
                    { currentToggledPlug
                        | askDeleteConfirmation = True
                    }
            in
            ( { model
                | toggledPlug = new_togglePlug
              }
            , Cmd.none
            )

        DeletePlug plugId ->
            ( { model
                | message = Just Message.Loading
              }
            , deletePlug token plugId
            )

        GotDeletedPlug plugId (Ok _) ->
            case model.plugs of
                RemoteData.Success plugs ->
                    let
                        new_plugs =
                            plugs
                                |> List.filter (\p -> (p.id == plugId) == False)
                    in
                    ( { model
                        | message = Just (Message.Success "Your plug has been deleted")
                        , plugs = RemoteData.Success new_plugs
                      }
                    , Message.fadeMessage FadeMessage
                    )

                _ ->
                    ( model, Nav.reload )

        GotDeletedPlug _ (Err error) ->
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

        TogglePlugActions plugId ->
            case model.toggledPlug.plug of
                Just pid ->
                    case pid == plugId of
                        True ->
                            ( { model
                                | toggledPlug =
                                    { plug = Nothing
                                    , askDeleteConfirmation = False
                                    }
                              }
                            , Cmd.none
                            )

                        False ->
                            ( { model
                                | toggledPlug =
                                    { plug = Just plugId
                                    , askDeleteConfirmation = False
                                    }
                              }
                            , setToggledPlugIds plugId
                            )

                Nothing ->
                    ( { model
                        | toggledPlug =
                            { plug = Just plugId
                            , askDeleteConfirmation = False
                            }
                      }
                    , setToggledPlugIds plugId
                    )

        GotPlugs response ->
            case response of
                RemoteData.Failure (Http.BadStatus 401) ->
                    ( model
                    , Cmd.batch
                        [ Api.deleteToken ()
                        , Nav.load "/login"
                        ]
                    )

                _ ->
                    ( { model
                        | plugs = response
                      }
                    , Cmd.none
                    )

        OpenCreatePlug Nothing ->
            ( model, Nav.pushUrl navKey "/app/create-plug" )

        FadeMessage ->
            ( { model | message = Nothing }, Cmd.none )

        _ ->
            ( model, Cmd.none )
