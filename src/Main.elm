port module Main exposing (..)

import Html exposing (Html, programWithFlags, div, text, button)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


initialCards : List Card
initialCards =
    [ { korean = "ㄱ", english = "g", interval = 1 }
    , { korean = "ㄴ", english = "n", interval = 1 }
    , { korean = "ㄷ", english = "d", interval = 1 }
    , { korean = "ㄹ", english = "l/r", interval = 1 }
    , { korean = "ㅁ", english = "m", interval = 1 }
    , { korean = "ㅂ", english = "b", interval = 1 }
    , { korean = "ㅅ", english = "s", interval = 1 }
    , { korean = "ㅇ", english = "silent (top), ng (bottom)", interval = 1 }
    , { korean = "ㅈ", english = "j", interval = 1 }
    , { korean = "ㅊ", english = "ch", interval = 1 }
    , { korean = "ㅋ", english = "k", interval = 1 }
    , { korean = "ㅌ", english = "t", interval = 1 }
    , { korean = "ㅍ", english = "p", interval = 1 }
    , { korean = "ㅎ", english = "h", interval = 1 }
    , { korean = "ㅏ", english = "a", interval = 1 }
    , { korean = "ㅓ", english = "eo", interval = 1 }
    , { korean = "ㅗ", english = "o", interval = 1 }
    , { korean = "ㅜ", english = "u", interval = 1 }
    , { korean = "ㅡ", english = "eu", interval = 1 }
    , { korean = "ㅣ", english = "i", interval = 1 }
    , { korean = "ㅑ", english = "ya", interval = 1 }
    , { korean = "ㅕ", english = "yeo", interval = 1 }
    , { korean = "ㅛ", english = "yo", interval = 1 }
    , { korean = "ㅠ", english = "yu", interval = 1 }
    ]


type alias Card =
    { english : String
    , korean : String
    , interval : Int
    }


type Options
    = EnglishFirst
    | KoreanFirst


type alias Model =
    { cards : List Card
    , backShown : Bool
    , menuShown : Bool
    , options : Options
    }


type Msg
    = Start
    | Stop
    | ShowBack
    | NextCard Bool
    | ToggleOptions
    | Reset


firstCard : List Card -> Card
firstCard cards =
    cards
        |> List.head
        |> Maybe.withDefault (Card "" "" 0)


view : Model -> Html Msg
view model =
    div [ mainStyle ] <|
        if model.menuShown then
            [ viewMenu model.options ]
        else
            [ viewProgressBar model.cards
            , viewCard (firstCard model.cards) model.backShown model.options
            ]


viewMenu : Options -> Html Msg
viewMenu options =
    div []
        [ div [ style [ ( "text-align", "center" ) ] ]
            [ text "Korean Alphabet in 15 Minutes" ]
        , button [ btnResetStyle, onClick Reset ] [ text "Reset Data" ]
        , case options of
            EnglishFirst ->
                div [ btnOptionStyle options, onClick ToggleOptions ] [ text "Front: 🇺🇸" ]

            KoreanFirst ->
                div [ btnOptionStyle options, onClick ToggleOptions ] [ text "Front: 🇰🇷" ]
        , button [ style btnStyle, onClick Start ] [ text "Start" ]
        ]


viewCard : Card -> Bool -> Options -> Html Msg
viewCard card backShown options =
    if backShown then
        div [] <|
            (case options of
                EnglishFirst ->
                    [ div [ centerStyle ] [ text card.english ]
                    , div [ centerStyle ] [ text card.korean ]
                    ]

                KoreanFirst ->
                    [ div [ centerStyle ] [ text card.korean ]
                    , div [ centerStyle ] [ text card.english ]
                    ]
            )
                ++ [ button [ style leftBtnStyle, onClick <| NextCard True ] [ text "✔" ]
                   , button [ style rightBtnStyle, onClick <| NextCard False ] [ text "✖" ]
                   ]
    else
        div []
            [ div [ centerStyle ]
                [ text <|
                    case options of
                        EnglishFirst ->
                            card.english

                        KoreanFirst ->
                            card.korean
                ]
            , button [ style btnStyle, onClick ShowBack ] [ text "Show" ]
            ]


viewProgressBar : List Card -> Html Msg
viewProgressBar cards =
    let
        passed =
            cards
                |> List.filter (\card -> card.interval > 1)
                |> List.length

        total =
            cards |> List.length

        passedPercentage =
            round <| (toFloat passed) / (toFloat total) * 100

        unpassedPercentage =
            100 - passedPercentage
    in
        div [ progressBarStyle ]
            [ div [ progressBarLeftStyle passedPercentage ] []
            , div [ progressBarRightStyle unpassedPercentage ] []
            , div [ leftArrowStyle, onClick Stop ] [ text "◀" ]
            , div [ progressBarTextStyle ]
                [ text <| (toString passed) ++ " / " ++ (toString total) ]
            ]


mainStyle =
    style
        [ ( "font-family", "Arial" )
        , ( "font-size", "60px" )
        , ( "font-weight", "900" )
        , ( "color", "#000000" )
        , ( "background", "#f1f1f1" )
        , ( "width", "400px" )
        , ( "height", "600px" )
        , ( "margin", "auto" )
        , ( "position", "relative" )
        ]


centerStyle =
    style
        [ ( "text-align", "center" )
        , ( "padding", "50px" )
        ]


btnStyle =
    [ ( "font-size", "60px" )
    , ( "font-weight", "900" )
    , ( "background", "#1bb8d6" )
    , ( "border-width", "0" )
    , ( "padding", "10px" )
    , ( "position", "absolute" )
    , ( "bottom", "0" )
    , ( "width", "100%" )
    ]


leftBtnStyle =
    btnStyle
        ++ [ ( "width", "50%" )
           , ( "left", "0" )
           , ( "background", "#77D27E" )
           ]


rightBtnStyle =
    btnStyle
        ++ [ ( "width", "50%" )
           , ( "right", "0" )
           , ( "background", "rgba(182, 0, 0, 0.8)" )
           ]


btnResetStyle =
    style <|
        btnStyle
            ++ [ ( "text-align", "center" )
               , ( "bottom", "185px" )
               ]


btnOptionStyle options =
    style <|
        btnStyle
            ++ [ ( "text-align", "center" )
               , ( "bottom", "90px" )
               , ( "width", "95%" )
               , ( "background"
                 , case options of
                    EnglishFirst ->
                        "#77D27E"

                    KoreanFirst ->
                        "rgba(182, 0, 0, 0.8)"
                 )
               ]


progressBarStyle =
    style
        [ ( "position", "relative" )
        ]


progressBarLeftStyle : Int -> Html.Attribute Msg
progressBarLeftStyle percentage =
    style
        [ ( "position", "absolute" )
        , ( "background", "#77D27E" )
        , ( "left", "0" )
        , ( "top", "0" )
        , ( "height", "70px" )
        , ( "width", (toString percentage) ++ "%" )
        ]


progressBarRightStyle : Int -> Html.Attribute Msg
progressBarRightStyle percentage =
    style
        [ ( "position", "absolute" )
        , ( "background", "rgba(182, 0, 0, 0.8)" )
        , ( "right", "0" )
        , ( "top", "0" )
        , ( "height", "70px" )
        , ( "width", (toString percentage) ++ "%" )
        ]


leftArrowStyle =
    style
        [ ( "position", "absolute" )
        , ( "font-size", "45px" )
        , ( "left", "10px" )
        , ( "z-index", "1" )
        ]


progressBarTextStyle =
    style
        [ ( "text-align", "center" )
        , ( "position", "relative" )
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            { model | menuShown = False, backShown = False } ! []

        Stop ->
            { model | menuShown = True, backShown = False } ! []

        ShowBack ->
            { model | backShown = True } ! []

        NextCard isPassing ->
            let
                testedCard =
                    firstCard model.cards

                otherCards =
                    model.cards
                        |> List.tail
                        |> Maybe.withDefault []

                newInterval =
                    if isPassing then
                        if testedCard.interval >= 13 then
                            testedCard.interval + 6
                        else
                            testedCard.interval + 3
                    else
                        1

                newCards =
                    (List.take newInterval otherCards)
                        ++ [ { testedCard | interval = newInterval } ]
                        ++ (List.drop newInterval otherCards)
            in
                ( { model | backShown = False, cards = newCards }
                , save <|
                    SaveData
                        newCards
                        (case model.options of
                            KoreanFirst ->
                                "KoreanFirst"

                            _ ->
                                "EnglishFirst"
                        )
                )

        ToggleOptions ->
            let
                newOptions =
                    case model.options of
                        EnglishFirst ->
                            KoreanFirst

                        KoreanFirst ->
                            EnglishFirst
            in
                { model | options = newOptions } ! []

        Reset ->
            ( { model | cards = initialCards, options = EnglishFirst }
            , save <| SaveData initialCards "EnglishFirst"
            )


port save : SaveData -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias SaveData =
    { cards : List Card
    , options : String
    }


type alias Flags =
    { cards : Maybe (List Card)
    , options : Maybe String
    }


init : Flags -> ( Model, Cmd Msg )
init { cards, options } =
    Model
        (cards |> Maybe.withDefault initialCards)
        False
        True
        (case options of
            Just "KoreanFirst" ->
                KoreanFirst

            _ ->
                EnglishFirst
        )
        ! []


main =
    programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
