port module Page.Onboarding exposing (Model, Msg, init, update, view)

import Html exposing (Html, button, div, h1, h3, hr, img, input, li, section, span, text, ul)
import Html.Attributes exposing (class, href, id, src, style)
import Html.Events exposing (onClick)


type Country
    = India
    | RoW


type alias Model =
    { location : Country
    }


type Msg
    = ChangeCountry Country
    | MakePayment


init : ( Model, Cmd Msg )
init =
    let
        model =
            { location = RoW }
    in
    ( model, Cmd.none )


port makePayment : Int -> Cmd msg


body : Model -> Html Msg
body model =
    let
        charges =
            case model.location of
                India ->
                    "₹750 X 12"

                RoW ->
                    "$10 X 12"

        total =
            case model.location of
                India ->
                    "₹9000"

                RoW ->
                    "$120"
    in
    div [ class "container" ]
        [ section [ class "panels" ]
            [ section [ class "onboarding-panel" ]
                [ div [ class "onboarding" ]
                    [ div [ class "columns" ]
                        [ div [ class "heading-payment" ] [ text "Start your subscription" ]
                        , span []
                            [ text """
                            Your 7 days free trial has expired.
 
                            Kindly make a payment to continue using our tool further to have your social media game on point!

                            We currently offer an annual plan only.
                            """ ]
                        ]
                    , div [ class "columns" ]
                        [ span
                            [ style "font-weight" "600"
                            , style "font-size" "1rem"
                            , style "margin-bottom" "0.5rem"
                            ]
                            [ text "Select your location" ]
                        , div [ class "country-selection" ]
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
                        , span
                            [ style "font-weight" "600"
                            , style "font-size" "1rem"
                            ]
                            [ text "Summary" ]
                        , div [ class "total" ]
                            [ div [ class "charges" ]
                                [ span [ class "items" ] [ text "Charges" ]
                                , span [ class "items" ] [ text charges ]
                                ]
                            , hr [] []
                            , div [ class "charges" ]
                                [ span [ class "items" ] [ text "Total" ]
                                , span [ class "items" ] [ text total ]
                                ]
                            ]
                        , span [ class "payment", onClick MakePayment ] [ text "Make payment" ]
                        ]
                    ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ div [ class "message nothing" ] []
        , body model
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeCountry country ->
            ( { model
                | location = country
              }
            , Cmd.none
            )

        MakePayment ->
            let
                cmd =
                    case model.location of
                        India ->
                            makePayment 1

                        RoW ->
                            makePayment 2
            in
            ( model, cmd )
