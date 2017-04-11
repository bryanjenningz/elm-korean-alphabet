import Html exposing (Html, program, div, text, button)
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
  , { korean = "ㅇ", english = "null (initial)/ng (final)", interval = 1 }
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
  div [] [ viewCard (firstCard model.cards) model.backShown ]

viewCard : Card -> Bool -> Html Msg
viewCard card backShown =
  if backShown then
    div []
      [ div [] [ text card.english ]
      , div [] [ text card.korean ]
      , button [ onClick <| NextCard True ] [ text "Pass" ]
      , button [ onClick <| NextCard False ] [ text "Fail" ]
      ]
  else
    div []
      [ div [] [ text card.english ]
      , button [ onClick ShowBack ] [ text "Show" ]
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
          if isPassing then testedCard.interval + 3 else 1
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
