module SimpleSimple exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (id, value)
import Html.Events exposing (onClick, onInput)

type alias Model =
    { counter : Int
    , input : String
    , name : Maybe String
    }

type Msg
    = Decrement Int
    | Increment Int
    | OnInput String
    | Greet

init : Model
init =
    { counter = 0
    , input = ""
    , name = Nothing
    }

view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [] [ Html.text "Counter" ]
        , Html.button
            [ onClick (Decrement 20) ]
            [ Html.text "-2" ]
        , Html.button
            [ onClick (Decrement 1) ]
            [ Html.text "-1" ]
        , Html.span []
            [ Html.text (String.fromInt model.counter) ]
        , Html.button
            [ onClick (Increment 11) ]
            [ Html.text "+1" ]
        , Html.button
            [ onClick (Increment 2) ]
            [ Html.text "+2" ]
        , Html.br [] []
        , Html.br [] []
        , Html.br [] []
        , Html.br [] []
        , Html.br [] []
        , Html.div [] [ Html.text "Greetings" ]
        , Html.input [ onInput OnInput, value model.input ] []
        , Html.button [ onClick Greet ] [ Html.text "Dis bonjour" ]
        , Html.div [ id "test" ] <|
            case model.name of
                Nothing -> []
                Just name -> [ Html.text ("Bonjour " ++ name) ]
        ]

update : Msg -> Model -> Model
update msg model =
    case msg of
        Decrement value ->
            let newValue = model.counter - value in
            { model | counter = newValue }
        Increment value ->
            let newValue = model.counter + value in
            { model | counter = newValue }
        OnInput value ->
            { model | input = value }
        Greet ->
            let name = model.input in
            { model
                | name =
                    if name == "" then
                        Nothing
                    else
                        Just name
            }

main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
