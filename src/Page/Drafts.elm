port module Page.Drafts exposing (Model, Msg, init, subscriptions, update, view)

import Api
import Browser.Navigation as Nav
import Draft exposing (Draft, draftsDecoder)
import Html exposing (Attribute, Html, a, div, img, li, section, span, text, ul)
import Html.Attributes exposing (class, href, id, src, style)
import Html.Events exposing (onClick)
import Html.Keyed
import Http
import Json.Decode as D
import MessageBanner as Message exposing (MessageBanner)
import Page.Loading
import Page.Queue exposing (ToggledPost)
import Post exposing (PostId)
import Profile exposing (Avatar, Config)
import RemoteData exposing (WebData)
import Request
import Route exposing (Token)


type Msg
    = FadeMessage
    | OpenCreatePost (Maybe Int)
    | OpenEditDraft PostId
    | GotDrafts (WebData (List Draft))
    | TogglePostActions PostId
    | TurnOffDraftMenu
    | PromptDeleteDraft
    | DeletedDraft PostId
    | GotDeletedDraft Int (Result Http.Error ())


type alias Model =
    { drafts : WebData (List Draft)
    , toggledDraft : ToggledPost
    , message : MessageBanner
    }


deleteDraft : Token -> Int -> Cmd Msg
deleteDraft token postId =
    let
        decoder =
            D.succeed ()

        url =
            String.concat [ "/api/posts/", String.fromInt postId ]
    in
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "Authorization" token ]
        , url = url
        , body = Http.emptyBody
        , expect = Request.expectJson (GotDeletedDraft postId) decoder
        , timeout = Nothing
        , tracker = Nothing
        }


fetchDrafts : Token -> Cmd Msg
fetchDrafts token =
    let
        url =
            "/api/posts/drafts"
    in
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" token ]
        , url = url
        , body = Http.emptyBody
        , expect = Request.expectJson (RemoteData.fromResult >> GotDrafts) draftsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


init : Token -> ( Model, Cmd Msg )
init token =
    ( { drafts = RemoteData.Loading
      , toggledDraft =
            { post = Nothing
            , askDeleteConfirmation = False
            }
      , message = Nothing
      }
    , fetchDrafts token
    )


viewDraft : ToggledPost -> Avatar -> Draft -> Html Msg
viewDraft toggledDraft avatar draft =
    let
        postIdString =
            Post.postIdToInt draft.id
                |> String.fromInt

        settingsIconId =
            postIdString
                |> (++) "settingsIcon"

        postMenuId =
            postIdString
                |> (++) "postMenu"
    in
    li [ class "inqueue" ]
        [ img [ class "avatar", src (Page.Loading.getAvatarURL avatar) ] []
        , img [ class "site", src "/images/twitter.png" ] []
        , span [ class "content", onClick (OpenEditDraft draft.id) ] [ text draft.description ]
        , span [ class "time" ] [ text "" ]
        , img
            [ id settingsIconId
            , class "settings-icon"
            , src "/images/settings.svg"
            , onClick (TogglePostActions draft.id)
            ]
            []
        , span
            [ id postMenuId
            , class "post-menu"
            , style "display"
                (case toggledDraft.post of
                    Just postId ->
                        case Post.compareById postId draft.id of
                            True ->
                                "block"

                            False ->
                                "none"

                    Nothing ->
                        "none"
                )
            ]
            (case toggledDraft.askDeleteConfirmation of
                True ->
                    [ div [] [ text "Delete this draft?" ]
                    , ul [ class "settings-menu" ]
                        [ li [ onClick (DeletedDraft draft.id) ] [ text "Yes" ]
                        , li [ onClick TurnOffDraftMenu ] [ text "No" ]
                        ]
                    ]

                False ->
                    [ ul [ class "settings-menu" ]
                        [ li [ onClick (OpenEditDraft draft.id) ] [ text "Edit draft" ]
                        , li [ onClick PromptDeleteDraft ] [ text "Delete draft" ]
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
    case model.drafts of
        RemoteData.NotAsked ->
            Page.Loading.body

        RemoteData.Loading ->
            Page.Loading.body

        RemoteData.Success drafts ->
            case List.length drafts > 0 of
                True ->
                    div [ class "container" ]
                        [ section [ class "panels" ]
                            [ section [ class "subheader" ]
                                [ span [] [ text "Drafts" ]
                                , span [ onClick (OpenCreatePost Nothing) ] [ text "Create Post" ]
                                ]
                            , section [ class "one-panel" ]
                                [ div [ class "queue" ]
                                    [ div [ class "day" ]
                                        [ List.map
                                            (\draft ->
                                                ( Post.postIdToInt draft.id
                                                    |> String.fromInt
                                                , viewDraft model.toggledDraft avatar draft
                                                )
                                            )
                                            drafts
                                            |> Html.Keyed.ul []
                                        ]
                                    ]
                                ]
                            ]
                        ]

                False ->
                    div [ class "container" ]
                        [ section [ class "panels" ]
                            [ Page.Loading.emptyState 80 "You haven't saved any drafts yet"
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


port setToggledDraftIds : Int -> Cmd msg


port turnOffDraftMenu : (() -> msg) -> Sub msg


subscriptions : Sub Msg
subscriptions =
    turnOffDraftMenu (always TurnOffDraftMenu)


update : Msg -> Model -> Token -> Nav.Key -> ( Model, Cmd Msg )
update msg model token navKey =
    case msg of
        TurnOffDraftMenu ->
            ( { model
                | toggledDraft =
                    { post = Nothing
                    , askDeleteConfirmation = False
                    }
              }
            , Cmd.none
            )

        OpenEditDraft (Post.PostId postId) ->
            ( model, Nav.pushUrl navKey (String.concat [ "/app/drafts/", String.fromInt postId ]) )

        PromptDeleteDraft ->
            let
                currentToggledPost =
                    model.toggledDraft

                new_togglePost =
                    { currentToggledPost
                        | askDeleteConfirmation = True
                    }
            in
            ( { model
                | toggledDraft = new_togglePost
              }
            , Cmd.none
            )

        DeletedDraft (Post.PostId postId) ->
            ( { model
                | message = Just Message.Loading
              }
            , deleteDraft token postId
            )

        GotDeletedDraft postId (Ok _) ->
            case model.drafts of
                RemoteData.Success drafts ->
                    let
                        new_drafts =
                            drafts
                                |> List.filter (\p -> Post.compareById p.id (Post.PostId postId) == False)
                    in
                    ( { model
                        | message = Just (Message.Success "Your draft has been deleted")
                        , drafts = RemoteData.Success new_drafts
                      }
                    , Message.fadeMessage FadeMessage
                    )

                _ ->
                    ( model, Nav.reload )

        GotDeletedDraft _ (Err error) ->
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

        TogglePostActions postId ->
            case model.toggledDraft.post of
                Just pid ->
                    case Post.compareById pid postId of
                        True ->
                            ( { model
                                | toggledDraft =
                                    { post = Nothing
                                    , askDeleteConfirmation = False
                                    }
                              }
                            , Cmd.none
                            )

                        False ->
                            ( { model
                                | toggledDraft =
                                    { post = Just postId
                                    , askDeleteConfirmation = False
                                    }
                              }
                            , setToggledDraftIds (Post.postIdToInt postId)
                            )

                Nothing ->
                    ( { model
                        | toggledDraft =
                            { post = Just postId
                            , askDeleteConfirmation = False
                            }
                      }
                    , setToggledDraftIds (Post.postIdToInt postId)
                    )

        GotDrafts response ->
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
                        | drafts = response
                      }
                    , Cmd.none
                    )

        OpenCreatePost Nothing ->
            ( model, Nav.pushUrl navKey "/app/create" )

        FadeMessage ->
            ( { model | message = Nothing }, Cmd.none )

        _ ->
            ( model, Cmd.none )
