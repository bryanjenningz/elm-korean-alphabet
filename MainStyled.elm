import Html exposing (Html, program, div, text, button)
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
  , { korean = "ㅇ", english = "null (initial), ng (final)", interval = 1 }
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

type alias Model =
  { cards : List Card
  , backShown : Bool
  }

type Msg
  = ShowBack
  | NextCard Bool

firstCard : List Card -> Card
firstCard cards =
  cards
    |> List.head
    |> Maybe.withDefault (Card "" "" 0)

view : Model -> Html Msg
view model =
  div [ mainStyle ]
    [ viewProgressBar model.cards
    , viewCard (firstCard model.cards) model.backShown
    ]

viewCard : Card -> Bool -> Html Msg
viewCard card backShown =
  if backShown then
    div []
      [ div [ centerStyle ] [ text card.english ]
      , div [ centerStyle ] [ text card.korean ]
      , button [ style leftBtnStyle, onClick <| NextCard True ] [ text "✔" ]
      , button [ style rightBtnStyle, onClick <| NextCard False ] [ text "✖" ]
      ]
  else
    div []
      [ div [ centerStyle ] [ text card.english ]
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
      , div [ progressBarTextStyle ]
          [ text <| (toString passed) ++ " / " ++ (toString total) ]
      ]

mainStyle =
  style
    [ ("font-family", "Arial")
    , ("font-size", "60px")
    , ("font-weight", "900")
    , ("color", "#000000")
    , ("background", "#f1f1f1")
    , ("width", "400px")
    , ("height", "600px")
    , ("margin", "auto")
    , ("position", "relative")
    ]

centerStyle =
  style
    [ ("text-align", "center")
    , ("padding", "50px")
    ]

btnStyle =
  [ ("font-size", "60px")
  , ("font-weight", "900")
  , ("background", "#1bb8d6")
  , ("border-width", "0")
  , ("padding", "10px")
  , ("position", "absolute")
  , ("bottom", "0")
  , ("width", "100%")
  ]

leftBtnStyle =
  btnStyle ++
    [ ("width", "50%")
    , ("left", "0")
    , ("background", "#77D27E")
    ]

rightBtnStyle =
  btnStyle ++
    [ ("width", "50%")
    , ("right", "0")
    , ("background", "rgba(182, 0, 0, 0.8)")
    ]

progressBarStyle =
  style
    [ ("position", "relative")
    ]

progressBarLeftStyle : Int -> Html.Attribute Msg
progressBarLeftStyle percentage =
  style
    [ ("position", "absolute")
    , ("background", "#77D27E")
    , ("left", "0")
    , ("top", "0")
    , ("height", "70px")
    , ("width", (toString percentage) ++ "%")
    ]

progressBarRightStyle : Int -> Html.Attribute Msg
progressBarRightStyle percentage =
  style
    [ ("position", "absolute")
    , ("background", "rgba(182, 0, 0, 0.8)")
    , ("right", "0")
    , ("top", "0")
    , ("height", "70px")
    , ("width", (toString percentage) ++ "%")
    ]

progressBarTextStyle =
  style
    [ ("text-align", "center")
    , ("position", "relative")
    ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
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
          (List.take newInterval otherCards) ++
            [ { testedCard | interval = newInterval } ] ++
              (List.drop newInterval otherCards)
      in
        { model | backShown = False, cards = newCards } ! []

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

main =
  program
    { init = Model initialCards False ! []
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
