module Page.Loading exposing (body, emptyState, getAvatarURL, header, loadingView, redirection)

import Html exposing (Attribute, Html, a, div, img, li, section, span, text, ul)
import Html.Attributes exposing (class, href, id, src, style)
import Html.Events exposing (onClick)
import Profile exposing (Config)
import RemoteData exposing (WebData)
import Route exposing (Route)


checkIfActive : Route -> String -> List (Attribute msg)
checkIfActive route menuName =
    case ( route, menuName ) of
        ( Route.Index, "Queue" ) ->
            [ class "active" ]

        ( Route.Drafts, "Drafts" ) ->
            [ class "active" ]

        ( _, _ ) ->
            []


getHref : String -> String
getHref navItem =
    case navItem of
        "Queue" ->
            "/app/"

        "Drafts" ->
            "/app/drafts"

        _ ->
            "/app/"


getAvatarURL : Maybe String -> String
getAvatarURL avatar =
    case avatar of
        Just url ->
            url

        Nothing ->
            "/images/user.png"


headerNavItem : Route -> String -> Html msg
headerNavItem route navItem =
    a [ href (getHref navItem) ]
        [ li (checkIfActive route navItem)
            [ text navItem ]
        ]


header : Route -> Maybe String -> Bool -> ( msg, msg ) -> Html msg
header route avatar visible ( toggle, logout ) =
    section [ class "header" ]
        [ img
            [ class "logo"
            , src "/images/logo-small_without_bg.png"
            ]
            []
        , ul [ class "nav" ] <|
            List.map (headerNavItem route) <|
                [ {--"Queue", "Board", "Categories", "Plugs"--}
                  "Queue"
                , "Drafts"
                ]
        , span [ class "logo-wrapper" ]
            [ img
                [ id "user-avatar"
                , class "avatar-header"
                , onClick toggle
                , src (getAvatarURL avatar)
                ]
                []
            , span
                [ class "post-menu"
                , id "menu"
                , style "display"
                    (case visible of
                        True ->
                            "block"

                        False ->
                            "none"
                    )
                ]
                [ ul [ class "settings-menu" ]
                    [ li []
                        [ a [ href "/app/settings" ]
                            [ text "Settings"
                            ]
                        ]
                    , li [ onClick logout ] [ text "Logout" ]
                    ]
                ]

            {---
            , div
                [ class "loading-avatar" ]
                []
            ---}
            ]
        ]


body : Html msg
body =
    div [ class "loading-container" ]
        [ div [ style "display" "flex", style "flex-direction" "column", style "align-items" "center", style "row-gap" "30px", style "font-family" "'Work Sans', sans-serif" ]
            [ img [ src "/images/logo_gray.png", style "width" "12rem", style "justify-content" "center" ] []
            , div [ class "progress-bar" ]
                [ div [ class "progress-bar-value" ] []
                ]
            ]
        ]


redirection : Int -> String -> Html msg
redirection height message =
    div [ class "loading-container", style "height" (String.fromInt height ++ "vh") ]
        [ div [ style "display" "flex", style "flex-direction" "column", style "align-items" "center", style "row-gap" "30px", style "font-family" "'Work Sans', sans-serif" ]
            [ img [ src "/images/logo_gray.png", style "width" "12rem", style "justify-content" "center" ] []
            , span [] [ text message ]
            , div [ class "progress-bar" ]
                [ div [ class "progress-bar-value" ] []
                ]
            ]
        ]


emptyState : Int -> String -> Html msg
emptyState height message =
    div [ class "loading-container", style "height" (String.fromInt height ++ "vh") ]
        [ div [ style "display" "flex", style "flex-direction" "column", style "align-items" "center", style "row-gap" "30px", style "font-family" "'Work Sans', sans-serif" ]
            [ img [ src "/images/logo_gray.png", style "width" "12rem", style "justify-content" "center" ] []
            , span [] [ text message ]
            ]
        ]


loadingView : WebData Config -> Html msg
loadingView config =
    div []
        [ div [ class "message nothing" ] []
        , body
        ]
