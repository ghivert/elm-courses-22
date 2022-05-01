module Main exposing (..)

import Browser
import Html exposing (Html, div, text, button)
import Html.Events exposing (onClick)
import Html.Attributes exposing (id)

type alias Model =
  { todos : List String }

type Msg
  = ButtonClick
  | ClearList

initialModel : Model
initialModel =
  { todos =
    [ "Dire bonjour"
    , "Say hello"
    ]
  }

update : Msg -> Model -> Model
update msg model =
  case msg of
    ButtonClick ->
      { model |
        todos = "Rajoute un element" :: model.todos }
    ClearList ->
      { model | todos = [] }

viewTodo : String -> Html Msg
viewTodo todo =
  Html.p [] [ text todo ]

view : Model -> Html Msg
view model =
  let todos = (List.map viewTodo model.todos) in
  div
    [ id "First" ]
    [ div [ id "Second" ] todos
    , button [ onClick ButtonClick ] [ text "click me!" ]
    , button [ onClick ClearList ] [ text "click me too!" ]
    ]

main : Program () Model Msg
main =
  Browser.sandbox
    { init = initialModel
    , update = update
    , view = view
    }
