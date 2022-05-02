module Strings exposing (..)

-- Change that function body
isPalyndrome : String -> Bool
isPalyndrome str =
  if str == "" then True else
  let len = String.length str
      firstChar = String.slice 0 1 str
      endChar = String.slice (len - 1) len str in
    firstChar == endChar && isPalyndrome (String.slice 1 (len - 1) str)

alph : List String
alph = String.split "" "abcdefghijklmnopqrstuvwxyz"

-- Change that function body
isPangramHelp : String -> List String -> Bool
isPangramHelp str alphabet =
  case alphabet of
    [] -> True
    letter :: rest ->
      if String.contains letter str then
        isPangramHelp str rest
      else
        False

isPangram : String -> Bool
isPangram str = isPangramHelp str alph

isPangramAny : String -> Bool
isPangramAny str = List.any (\l -> String.contains l str) alph
