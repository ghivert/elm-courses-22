"Ada Lovelace"
  |> String.split " "
  |> List.tail -- Just ([ "Lovelace" ]) -- Maybe (List String)

  |> Maybe.map (List.head) -- Just (Just "Lovelace")

  |> (\m -> -- Maybe (List String) -> Maybe String
      case m of
        Nothing -> Nothing
        Just value -> List.head value) -- Just "Lovelace"

        -- (List String -> Maybe String)
        --    -> Maybe (List String)
        --    -> Maybe String

  |> (\fun m ->
      case m of -- Maybe String
        Nothing -> Nothing -- Maybe String
        Just value -> fun value -- Maybe String
     ) List.head -- Maybe (List String) -> Maybe String
      -- Just "Lovelace"

-- (List String -> Maybe String) -> Maybe (List String) -> Maybe String
(\fun m ->
    case m of -- Maybe String
      Nothing -> Nothing -- Maybe String
      Just value -> fun value -- Maybe String
   ) (List.head) (
        List.tail (
            String.split " " "Ada Lovelace")
     )

-- (a -> Maybe b) -> Maybe (a) -> Maybe b
bind fun m =
  case m of -- Maybe a
    Nothing -> Nothing -- Maybe b
    Just value -> fun value -- Maybe b
