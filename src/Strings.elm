module Strings exposing (..)

-- Change that function body
isPalyndrome : String -> Bool
isPalyndrome str =
  if str == "" then
    True
  else
    let
      firstChar = String.slice 0 1 str
      endChar =
        String.slice
          (String.length str - 1)
          (String.length str)
          str
    in
      if firstChar == endChar then
        isPalyndrome
          (String.slice 1
            (String.length str - 1) str
          )
      else
        False

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
isPangram str =
  isPangramHelp str alph

isPangramAny : String -> Bool
isPangramAny str =
  List.any
    (\letter -> String.contains letter str)
    alph
