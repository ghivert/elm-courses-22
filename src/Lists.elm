module Lists exposing (..)

-- Compute the length of a List without using List.length.
-- Change function body.
length : List a -> Int
length li =
  case li of
    [] -> 0
    hd :: tl -> 1 + (length tl)

-- Checks if a value is includes in the List without using List.member.
-- Change function body.
member : Int -> List Int -> Bool
member num li =
  case li of
    [] -> False
    hd :: tl -> num == hd || member num tl

-- Removes all elements at an even position in the List.
-- Change function body.
filterEven : List a -> List a
filterEven li =
  let
    filter li =
      case li of
        [] -> []
        (index, elem) ->
          if index |> modBy 2 == 0 then
            filter elem
          else
            elem :: filter elem
  in
    li
    |> List.indexedMap Tuple.pair
    |> filter

-- Removes all elements not satistfying the predicate.
-- Change function body.
filter : (a -> Bool) -> List a -> List b
filter fun =
  let
    predicate elem =
      case elem of
        True -> [ elem ]
        False -> []
  in
    List.concatMap predicate

-- Computes the sum of a list of Int.
-- Change function body.
sum : List Int -> Int
sum list = List.foldl (+) 0

recSum : List Int -> Int
recSum =
  let
    sumHelp acc li =
      case li of
        [] -> acc
        hd :: tl -> sumHelp (acc + hd) tl
  in
    sumHelp 0

-- Append the second list to the first without using List.append.
-- Change function body.
append : List a -> List a -> List a
append li1 li2 = List.foldr (::) li1 li2
