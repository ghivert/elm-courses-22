module Simple exposing (..)

import Browser
import Html exposing (Html)
import Html.Events as Events

type alias Model = Int

type Msg
  = Increment Int
  | Decrement Int
  | Reset

main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , view = view
    , update = update
    }

init : Model
init = 0

view : Model -> Html Msg
view model =
  Html.div []
    [ Html.div [] [ Html.text (String.fromInt model) ]
    , Html.button [ Events.onClick (Increment 1) ] [ Html.text "+1" ]
    , Html.button [ Events.onClick (Decrement 1) ] [ Html.text "-1" ]
    , Html.button [ Events.onClick (Increment 2) ] [ Html.text "+2" ]
    , Html.button [ Events.onClick (Decrement 2) ] [ Html.text "-2" ]
    , Html.button [ Events.onClick Reset ] [ Html.text "Reset" ]
    ]

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment value -> model + value
    Decrement value -> model - value
    Reset -> 0
