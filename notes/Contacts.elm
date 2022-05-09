module Contacts exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events

type alias Contact =
  { firstName : String
  , lastName : String
  , phoneNumber : String
  }

type alias Model =
  { contacts : List Contact
  , firstName : String
  , lastName : String
  , phoneNumber : String
  , search : String
  , edit : Maybe String
  , favorites : List String
  }

type Msg
  = InputFirstName String
  | InputLastName String
  | InputPhone String
  | AddContact
  | SuppressContact String
  | Search String
  | Edit
  | MakeEditable Contact
  | Favorite String

init : Model
init = Model [] "" "" "" "" Nothing []

appendContact : Model -> List Contact
appendContact ({ contacts, firstName, lastName, phoneNumber } as model) =
  List.append contacts [ Contact firstName lastName phoneNumber ]

update : Msg -> Model -> Model
update msg model =
  case msg of
    InputFirstName first -> { model | firstName = first }
    InputLastName last -> { model | lastName = last }
    InputPhone phoneNumber -> { model | phoneNumber = phoneNumber }
    AddContact ->
      { model
        | contacts = appendContact model
        , firstName = ""
        , lastName = ""
        , phoneNumber = ""
      }
    SuppressContact id ->
      { model
        | contacts =
          List.filter
            (\contact -> uniqueContact contact /= id)
            model.contacts
      }
    Search content ->
      { model | search = content }
    Edit ->
      { model | edit = Nothing }
      |> case model.edit of
        Nothing -> update AddContact
        Just contact ->
          \newModel ->
            newModel
            |> update (SuppressContact contact)
            |> update AddContact
    MakeEditable contact ->
      { model
        | firstName = contact.firstName
        , lastName = contact.lastName
        , phoneNumber = contact.phoneNumber
        , edit = Just (uniqueContact contact)
      }
    Favorite id ->
      if List.member id model.favorites then
        { model | favorites = List.filter ((/=) id) model.favorites }
      else
        { model | favorites = id :: model.favorites }

uniqueContact : Contact -> String
uniqueContact { firstName, lastName, phoneNumber } =
  [ firstName, lastName, phoneNumber ] |> String.join ","

viewContact : List String -> Contact -> Html Msg
viewContact favorites ({ firstName, lastName, phoneNumber } as contact) =
  let id = uniqueContact contact in
  Html.div []
    [ Html.div [] [ Html.text firstName ]
    , Html.div [] [ Html.text lastName ]
    , Html.div [] [ Html.text phoneNumber ]
    , Html.div []
      [ Html.button
        [ Html.Events.onClick (MakeEditable contact) ]
        [ Html.text "Editer" ]
      ]
    , Html.div []
      [ Html.button
        [ Html.Events.onClick (SuppressContact (id)) ]
        [ Html.text "Supprimer" ]
      ]
    , Html.div []
      [ Html.button
        [ Html.Events.onClick (Favorite id) ]
        [ Html.text
          (if List.member (id) favorites then
            "Retirer des favoris"
           else
             "Ajouter aux favoris"
          )
        ]
      ]
    ]

exactFilter : String -> Contact -> Bool
exactFilter search { firstName, lastName, phoneNumber } =
  search == firstName
    || search == lastName
    || search == phoneNumber

filterBySearch : String -> Contact -> Bool
filterBySearch search { firstName, lastName, phoneNumber } =
  String.startsWith search firstName
    || String.startsWith search lastName
    || String.startsWith search phoneNumber

proposeContact : Contact -> Html Msg
proposeContact { firstName, lastName, phoneNumber } =
  Html.div []
    [ Html.div [] [ Html.text firstName ]
    , Html.div [] [ Html.text lastName ]
    , Html.div [] [ Html.text phoneNumber ]
    ]

view : Model -> Html Msg
view model =
  Html.div []
    <| List.concat
      [ List.map (viewContact model.favorites)
          (List.filter (exactFilter model.search)
            model.contacts
          )
      , [ Html.div []
          [ Html.input [ Html.Events.onInput InputFirstName ] []
          , Html.input [ Html.Events.onInput InputLastName ] []
          , Html.input [ Html.Events.onInput InputPhone ] []
          , Html.input
            [ Html.Attributes.type_ "submit"
            , Html.Events.onClick Edit
            ]
            []
          ]
        , Html.div []
          [ Html.input
            [ Html.Events.onInput Search
            , Html.Attributes.value model.search
            ]
            []
          ]
        , if model.search /= "" then
            Html.div []
              [ Html.div [] [ Html.text "Propositions" ]
              , Html.div []
                (List.map proposeContact
                  (List.filter (filterBySearch model.search)
                    model.contacts
                  )
                )
              ]
          else
            Html.text ""
        ]
      ]

main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , update = update
    , view = view
    }
