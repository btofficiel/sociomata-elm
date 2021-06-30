module Page.Loading exposing (body, emptyState, getAvatarURL, header, loadingView)

import Html exposing (Attribute, Html, div, img, li, section, span, text, ul)
import Html.Attributes exposing (class, src, style)
import Profile exposing (Config)
import RemoteData exposing (WebData)
import Route exposing (Route)


checkIfActive : Route -> String -> List (Attribute msg)
checkIfActive route menuName =
    case ( route, menuName ) of
        ( Route.Index, "Queue" ) ->
            [ class "active" ]

        ( _, _ ) ->
            []


getAvatarURL : Maybe String -> String
getAvatarURL avatar =
    case avatar of
        Just url ->
            url

        Nothing ->
            "https://pbs.twimg.com/profile_images/1307543302778970117/BvktyftG_400x400.jpg"


header : Route -> Maybe String -> Html msg
header route avatar =
    section [ class "header" ]
        [ img
            [ class "logo"
            , src "/images/logo-small_without_bg.png"
            ]
            []
        , ul [ class "nav" ] <|
            List.map (\mi -> li (checkIfActive route mi) [ text mi ]) <|
                [ "Queue", "Board", "Categories", "Plugs" ]
        , span [ class "logo-wrapper" ]
            [ img [ class "avatar-header", src (getAvatarURL avatar) ]
                []

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
        [ span [] [ text "sociomata" ]
        ]


emptyState : Int -> String -> Html msg
emptyState height message =
    div [ class "loading-container", style "height" (String.fromInt height ++ "vh") ]
        [ div [ style "display" "flex", style "flex-direction" "column", style "align-items" "center", style "row-gap" "30px", style "font-family" "'Work Sans', sans-serif" ]
            [ img [ src "/images/logo_gray.png", style "width" "12rem", style "justify-content" "center" ] []
            , span [] [ text message ]
            ]
        ]


loadingView : Route -> WebData Config -> Html msg
loadingView route config =
    div []
        [ header route (Profile.getAvatar config)
        , body
        ]
