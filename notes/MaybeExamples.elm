module MaybeExamples exposing (..)

entiers : List Int
entiers = [1, 2, 3, 4]

-- Cet exemple ne compile pas. Il faut se référer à Nothing.
first : Int
first = List.head entiers

empties : List Int
empties = []

-- Cet exemple compile. Il retourne Nothing.
nothing : Maybe Int
nothing =
  List.head empties

addOne : Int -> Int
addOne x =
  x + 1

-- Le type Maybe tel que défini dans la bibliothèque standard.
type Maybe a
  = Nothing
  | Just a

example : Maybe Int -> Maybe Int
example const =
  case const of
    Nothing -> Nothing
    Just value ->
      if value == 0 then
        Nothing
      else
        Just (addOne value)

safeDiv : Float -> Float -> Maybe Float
safeDiv upNumber downNumber =
  case downNumber of
    0.0 -> Nothing
    number -> Just (upNumber / downNumber)
