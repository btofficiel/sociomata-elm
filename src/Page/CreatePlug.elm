module Page.CreatePlug exposing (Model, Msg, init, update, view)

import Api
import Browser.Navigation as Nav
import Html exposing (Attribute, Html, button, div, img, input, li, option, section, select, span, text, textarea, ul)
import Html.Attributes as Attrs exposing (class, id, placeholder, src, style, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Http
import Json.Decode as D
import MessageBanner as Message exposing (MessageBanner)
import Page.CreatePost exposing (calculateTweetProgress, resetTextArea, resizeTextArea)
import Plug exposing (plugEncoder)
import Request
import Route exposing (Token)


type alias Model =
    { name : String
    , content : String
    , message : MessageBanner
    }


type Msg
    = EnterName String
    | EnterContent String
    | CreatePlug
    | GotCreatedPlug (Result Http.Error ())
    | RemoveContent
    | FadeMessage


createPlug :
    { name : String
    , content : String
    }
    -> Token
    -> Cmd Msg
createPlug model token =
    let
        decoder =
            D.succeed ()
    in
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" token ]
        , url = "/api/plugs"
        , body = Http.jsonBody (plugEncoder model)
        , expect = Request.expectJson GotCreatedPlug decoder
        , timeout = Nothing
        , tracker = Nothing
        }


init : ( Model, Cmd Msg )
init =
    ( { name = ""
      , content = ""
      , message = Nothing
      }
    , Cmd.none
    )


viewEditor : Model -> Html Msg
viewEditor model =
    let
        id_ =
            "createPlugEditor"

        contentLength =
            String.length model.content
    in
    div []
        [ div [ class "editor" ]
            [ textarea
                [ placeholder "Write your plug"
                , value model.content
                , id id_
                , onInput EnterContent
                ]
                []
            , img [ class "close", src "/images/closeHover.png", onClick RemoveContent ] []
            , div [ class "post-settings " ]
                [ span [ class "twitter-counter" ]
                    [ span []
                        [ text
                            (String.concat
                                [ String.fromInt contentLength
                                , "/280"
                                ]
                            )
                        ]
                    , span [ class "tweet-count-indicator" ]
                        [ span
                            [ class "tweet-count-progress"
                            , style "width"
                                (String.concat
                                    [ calculateTweetProgress contentLength
                                    , "rem"
                                    ]
                                )
                            ]
                            []
                        ]
                    ]
                ]
            ]
        ]


viewOptions : Model -> Html Msg
viewOptions model =
    div []
        [ div []
            [ span [ class "option-name-value" ]
                [ span [ class "w-option-name" ]
                    [ text "Plug name" ]
                , input
                    [ type_ "text"
                    , class "schedule-time"
                    , onInput EnterName
                    , value model.name
                    ]
                    []
                ]
            , span [ class "button" ]
                [ button [ onClick CreatePlug ] [ text "Create plug" ]
                ]
            ]
        ]


body : Model -> Html Msg
body model =
    div [ class "container" ]
        [ section [ class "creator-two-panels" ]
            [ div [ class "writer" ]
                [ section [ class "subheader" ]
                    [ span [] [ text "Create Plug" ]
                    ]
                , ul [] [ viewEditor model ]
                ]
            , div [ class "writer-options" ]
                [ viewOptions model
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ Message.viewMessage model.message
        , body model
        ]


update : Msg -> Model -> Token -> ( Model, Cmd Msg )
update msg model token =
    case msg of
        EnterName name ->
            ( { model
                | name = String.slice 0 25 name
              }
            , Cmd.none
            )

        EnterContent content ->
            let
                key =
                    "createPlugEditor"
            in
            ( { model
                | content = String.slice 0 280 content
              }
            , resizeTextArea key
            )

        RemoveContent ->
            let
                key =
                    "createPlugEditor"
            in
            ( { model
                | name = ""
                , content = ""
              }
            , resetTextArea key
            )

        CreatePlug ->
            let
                payload =
                    { name = model.name
                    , content = model.content
                    }
            in
            ( { model
                | message = Just Message.Loading
              }
            , createPlug payload token
            )

        FadeMessage ->
            ( { model | message = Nothing }, Cmd.none )

        GotCreatedPlug (Ok _) ->
            ( { model
                | message = Just (Message.Success "Yes! your plug has been created")
              }
            , Message.fadeMessage FadeMessage
            )

        GotCreatedPlug (Err error) ->
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
