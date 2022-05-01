module Lists exposing (..)

length : List a -> Int
length li =
  case li of
    [] -> 0
    hd :: tl -> 1 + (length tl)

member : Int -> List Int -> Bool
member num li =
  case li of
    [] -> False
    hd :: tl ->
      if num == hd then
        True
      else
        member num tl

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

filter : (a -> Bool) -> List a -> List b
filter fun =
  let
    predicate elem =
      case elem of
        True -> [ elem ]
        False -> []
  in
    List.concatMap predicate

sum : List Int -> Int
sum = List.foldl (+) 0

recSum : List Int -> Int
recSum =
  let
    sumHelp acc li =
      case li of
        [] -> acc
        hd :: tl -> sumHelp (acc + hd) tl
  in
    sumHelp 0

append : List a -> List a -> List a
append li1 li2 = List.foldr (::) li1 li2
