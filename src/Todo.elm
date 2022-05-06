module Todo exposing (..)

import Browser
import Html exposing (Html, div, text, input, button, label)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)

type alias ID = Int

type TodoState
  = Pending
  | Active
  | Done

todoStateToString : TodoState -> String
todoStateToString state =
  case state of
    Pending -> "Pending"
    Active -> "Active"
    Done -> "Done"

type alias Todo =
  { id : ID
  , title : String
  , content : String
  , state : TodoState
  }

type alias Model =
  { todos: List Todo
  , id : ID
  , title : String
  , content : String
  }

type Msg
  = OnInputTitle String
  | OnInputContent String
  | ButtonClick
  | UpdateTodoState TodoState ID

init : Model
init =
  { todos = []
  , id = 0
  , content = ""
  , title = ""
  }

updateTodo : TodoState -> Int -> Model -> Model
updateTodo state id model =
  { model
    | todos =
      List.map
        (\todo ->
          if todo.id == id then
            { todo | state = state }
          else
            todo
        )
        model.todos
  }

update : Msg -> Model -> Model
update msg model =
  case Debug.log "msg" msg of
    OnInputTitle title ->
      { model | title = title }
    OnInputContent content ->
      { model | content = content }
    ButtonClick ->
      let
        todo =
          { id = model.id
          , title = model.title
          , content = model.content
          , state = Pending
          }
      in
        { model
          | todos = todo :: model.todos
          , title = ""
          , content = ""
          , id = model.id + 1
        }
    UpdateTodoState state id ->
      updateTodo state id model

view : Model -> Html Msg
view model =
  div []
    ([ text "To-do list"
    , div []
      [ label []
        [ text "Title"
        , input [ onInput OnInputTitle, value model.title  ] []
        ]
      , label []
        [ text "Content"
        , input [ onInput OnInputContent, value model.content ] []
        ]
      ]
    , button [ onClick ButtonClick ] [ text "Submit" ]
    ] ++
      (List.map
        (\todo ->
          div []
            [ text "Todo: #"
            , text (String.fromInt todo.id)
            , text " "
            , text todo.title
            , text " "
            , text todo.content

            -- Display state
            , text " \""
            , text (todoStateToString todo.state)
            , text "\" "

            , button [ onClick (UpdateTodoState Pending todo.id) ] [ text "À venir" ]
            , button [ onClick (UpdateTodoState Active todo.id) ] [ text "En cours" ]
            , button [ onClick (UpdateTodoState Done todo.id) ] [ text "Terminée" ]
            ]
        )
        model.todos
      )
    )

main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , update = update
    , view = view
    }
