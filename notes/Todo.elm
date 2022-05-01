module Todo exposing (..)

import Browser
import Html exposing (Html)
import Html.Events as Events
import Html.Attributes as Attributes

type TodoState
  = Pending
  | Finished
  | Future

stateToString : TodoState -> String
stateToString state =
  case state of
    Pending -> "Pending"
    Finished -> "Finished"
    Future -> "Later"

type alias Todo =
  { state : TodoState
  , title : String
  , content : String
  }

type alias Model =
  { todos : List Todo
  , newForm :
    { title : String
    , content : String
    }
  , checked : List String
  }

type Msg
  = EditTitle String
  | EditContent String
  | AddTodo
  | Checkbox String Bool
  | InProgress
  | Done
  | Delete

main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , view = view
    , update = update
    }

init : Model
init = Model [] { title = "", content = "" } []

viewTodo : (Todo, Bool) -> Html Msg
viewTodo ({ state, title, content }, checked) =
  Html.div
    [ Attributes.style "display" "flex" ]
    [ Html.input
      [ Attributes.type_ "checkbox"
      , Attributes.checked checked
      , Events.onCheck (Checkbox title)
      ]
      []
    , Html.div []
      [ Html.div [] [ Html.text title ]
      , Html.div [] [ Html.text content ]
      , Html.div [] [ Html.text (stateToString state) ]
      ]
    ]

spacer : Html msg
spacer =
  Html.div [ Attributes.style "padding" "12px" ] []

view : Model -> Html Msg
view model =
  Html.div []
    [ Html.form
      [ Attributes.style "display" "flex"
      , Attributes.style "flex-direction" "column"
      , Attributes.style "maxWidth" "500px"
      , Events.onSubmit AddTodo
      ]
      [ Html.input
        [ Attributes.value model.newForm.title, Events.onInput EditTitle ]
        []
      , spacer
      , Html.textarea
        [ Attributes.value model.newForm.content, Events.onInput EditContent ]
        []
      , spacer
      , Html.input
        [ Attributes.type_ "submit" ]
        []
      ]
    , Html.button [ Events.onClick InProgress ] [ Html.text "En cours" ]
    , Html.button [ Events.onClick Done ] [ Html.text "Terminés" ]
    , Html.button [ Events.onClick Delete ] [ Html.text "Delete" ]
    , model.todos
      |> List.map (\todo -> (todo, List.member todo.title model.checked))
      |> List.map viewTodo
      |> List.intersperse spacer
      |> Html.div []
    ]

setState : List String -> TodoState -> Todo -> Todo
setState checked state todo =
  if List.member todo.title checked then
    { todo | state = state }
  else
    todo

update : Msg -> Model -> Model
update msg ({ newForm, checked } as model) =
  case msg of
    EditTitle title ->
      let newForm_ = { newForm | title = title } in
      { model | newForm = newForm_ }
    EditContent content ->
      let newForm_ = { newForm | content = content } in
      { model | newForm = newForm_ }
    AddTodo ->
      let newTodo = Todo Future newForm.title newForm.content
          newNewForm = { title = "", content = "" } in
        { model | todos = model.todos ++ [ newTodo ], newForm = newNewForm }
    Checkbox title state ->
      let
        newChecked =
          checked
          |> List.filter ((/=) title)
          |> if state then (::) title else identity
      in
        { model | checked = newChecked }
    InProgress ->
      { model | todos = List.map (setState model.checked Pending) model.todos }
    Done ->
      { model | todos = List.map (setState model.checked Finished) model.todos }
    Delete ->
      { model | todos = List.filter (\{ title } -> not (List.member title model.checked)) model.todos }
