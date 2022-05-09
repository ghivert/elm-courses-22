module Greetings exposing (..)

import Browser
import Html exposing (Html)
import Html.Events as Events
import Html.Attributes as Attributes

type alias Model =
  { buffer : String
  , greet : String
  }

type Msg
  = StringInput String
  | Greet
  | Reset

main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , view = view
    , update = update
    }

init : Model
init = Model "" ""

view : Model -> Html Msg
view model =
  Html.div [ Events.onSubmit Greet ]
    [ Html.input [ Attributes.value model.buffer, Events.onInput StringInput ] []
    , if model.greet == "" then
        Html.text ""
      else
        Html.text <| "Hello " ++ model.greet
    , Html.button [ Events.onClick Greet ] [ Html.text "Greet" ]
    , Html.button [ Events.onClick Reset ] [ Html.text "Reset" ]
    ]

update : Msg -> Model -> Model
update msg model =
  Debug.log "test" <|
  case msg of
    StringInput buf -> { model | buffer = buf }
    Greet -> { model | greet = model.buffer, buffer = "" }
    Reset -> { model | greet = "" }
