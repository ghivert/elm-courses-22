module Greetings exposing (..)

import Browser
import Html exposing (Html)
import Html.Events as Events
import Html.Attributes as Attributes

type alias Model =
  { first : Int
  , second : Int
  , result : Int
  }

type Msg
  = FirstInput String
  | SecondInput String
  | Add
  | Sub

main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , view = view
    , update = update
    }

init : Model
init = Model 0 0 0

view : Model -> Html Msg
view model =
  Html.div []
    [ Html.input
      [ Attributes.value (String.fromInt model.first)
      , Events.onInput FirstInput
      ]
      []
    , Html.input
      [ Attributes.value (String.fromInt model.second)
      , Events.onInput SecondInput
      ]
      []
    , Html.text <| "Result: " ++ (String.fromInt model.result)
    , Html.button [ Events.onClick Add ] [ Html.text "Add" ]
    , Html.button [ Events.onClick Sub ] [ Html.text "Sub" ]
    ]

update : Msg -> Model -> Model
update msg ({ first, second } as model) =
  case msg of
    FirstInput input ->
      case String.toInt input of
        Nothing -> model
        Just x -> { model | first = x }
    SecondInput input ->
      case String.toInt input of
        Nothing -> model
        Just x -> { model | second = x }
    Add -> { model | result = first + second }
    Sub -> { model | result = first - second }
