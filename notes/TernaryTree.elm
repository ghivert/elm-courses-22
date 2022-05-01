module Correction exposing (..)

import Html
import Set exposing (Set)

type TST
  = Empty
  | Node
    { value : Char
    , end : Bool
    , left : TST
    , right : TST
    , equal : TST
    }

empty : TST
empty = Empty

isEmpty : TST -> Bool
isEmpty tst =
  case tst of
    Empty -> True
    Node _ -> False

new : Char -> TST
new char =
  Node
    { value = char
    , end = False
    , left = empty
    , right = empty
    , equal = empty
    }

addLeft : TST -> TST -> TST
addLeft tst1 tst2 =
  case tst2 of
    Empty -> Empty
    Node value -> Node { value | left = tst1 }

addRight : TST -> TST -> TST
addRight tst1 tst2 =
  case tst2 of
    Empty -> Empty
    Node value -> Node { value | right = tst1 }

addEqual : TST -> TST -> TST
addEqual tst1 tst2 =
  case tst2 of
    Empty -> Empty
    Node value -> Node { value | equal = tst1 }

terminal : TST -> TST
terminal tst =
  case tst of
    Empty -> Empty
    Node value -> Node { value | end = True }

addTerminal : Bool -> TST -> TST
addTerminal bool =
  if bool then terminal else identity

insertHelp : List Char -> TST -> TST
insertHelp chars tst =
  case tst of
    Empty ->
      case chars of
        [] -> tst
        hd :: tl ->
          new hd
          |> addTerminal (List.length tl == 0)
          |> addEqual (insertHelp tl empty)
    Node value ->
      case chars of
        [] -> tst
        hd :: tl ->
          if hd == value.value then
            tst
            |> addTerminal (List.length tl == 0)
            |> addEqual (insertHelp tl value.equal)
          else if hd < value.value then
            addLeft (insertHelp chars value.left) tst
          else
            addRight (insertHelp chars value.right) tst

insert : String -> TST -> TST
insert word =
  insertHelp (String.toList word)

searchHelp : List Char -> TST -> Bool
searchHelp chars tst =
  case tst of
    Empty -> False
    Node value ->
      case chars of
        [] -> False
        hd :: tl ->
          if value.value == hd then
            if List.length tl == 0 then
              value.end
            else
              searchHelp tl value.equal
          else if hd < value.value then
            searchHelp chars value.left
          else
            searchHelp chars value.right

search : String -> TST -> Bool
search word =
  searchHelp (String.toList word)

removeEmptyNode : TST -> TST
removeEmptyNode tst =
  case tst of
    Empty -> Empty
    Node { end, left, right, equal } ->
      if isEmpty left && isEmpty equal && isEmpty right && not end then
        Empty
      else
        tst

deleteHelp : List Char -> TST -> TST
deleteHelp chars tst =
  case tst of
    Empty -> Empty
    Node value ->
      case chars of
        [] -> tst
        hd :: tl ->
          if value.value == hd then
            if List.length tl == 0 then
              if isEmpty value.right && isEmpty value.equal && isEmpty value.left then
                Empty
              else
                Node { value | end = False }
            else
              tst
              |> addEqual (deleteHelp tl value.equal)
              |> removeEmptyNode
          else if hd < value.value then
            tst
            |> addLeft (deleteHelp chars value.left)
            |> removeEmptyNode
          else
            tst
            |> addRight (deleteHelp chars value.right)
            |> removeEmptyNode

delete : String -> TST -> TST
delete word =
  deleteHelp (String.toList word)

prefixMatchHelp : TST -> List String
prefixMatchHelp tst =
  case tst of
    Empty -> []
    Node value ->
      let adder = ((++) (String.fromChar value.value)) in
      List.concat
        [ prefixMatchHelp value.left
        , List.map adder (prefixMatchHelp value.equal)
        , prefixMatchHelp value.right
        , if value.end then [ String.fromChar value.value ] else []
        ]

findTree : List Char -> TST -> Maybe TST
findTree chars tst =
  case tst of
    Empty -> Nothing
    Node value ->
      case chars of
        [] -> Nothing
        hd :: tl ->
          if value.value == hd then
            if List.length tl == 0 then
              Just value.equal
            else
              findTree tl value.equal
          else if hd < value.value then
            findTree chars value.left
          else
            findTree chars value.right

prefixMatch : String -> TST -> List String
prefixMatch word tst =
  case findTree (String.toList word) tst of
    Nothing -> []
    Just prefix -> List.map ((++) word) (prefixMatchHelp prefix)

length : TST -> Int
length tst =
  case tst of
    Empty -> 0
    Node value ->
      let num = if value.end then 1 else 0 in
      num + length value.right + length value.left + length value.equal

values : TST -> List String
values =
  prefixMatch ""

foldlHelp : String -> (String -> a -> a) -> a -> TST -> a
foldlHelp prefix fun acc tst =
  case tst of
    Empty -> acc
    Node value ->
      let word = prefix ++ String.fromChar value.value in
      acc
      |> \acc1 -> foldlHelp prefix fun acc1 value.left
      |> (if value.end then fun word else identity)
      |> \acc2 -> foldlHelp word fun acc2 value.equal
      |> \acc3 -> foldlHelp prefix fun acc3 value.right

foldl : (String -> a -> a) -> a -> TST -> a
foldl =
  foldlHelp ""

toSet : TST -> Set String
toSet tst =
  tst
  |> values
  |> List.foldl Set.insert Set.empty

fromSet : Set String -> TST
fromSet set =
  set
  |> Set.toList
  |> List.foldl insert empty

type TSTG a
  = TSTG (a -> String) (TSTGInside a)

type TSTGInside a
  = EmptyG
  | NodeG
    { value : Char
    , end : Maybe a
    , left : TSTGInside a
    , right : TSTGInside a
    , equal : TSTGInside a
    }

emptyG : (a -> String) -> TSTG a
emptyG hash =
  TSTG hash EmptyG

addEqualG : TSTGInside a -> TSTGInside a -> TSTGInside a
addEqualG tst1 tst2 =
  case tst2 of
    EmptyG -> EmptyG
    NodeG value -> NodeG { value | equal = tst1 }

insertGHelp : a -> List Char -> TSTGInside a -> TSTGInside a
insertGHelp data chars tst =
  case tst of
    EmptyG ->
      case chars of
        [] -> tst
        hd :: tl ->
          if List.length tl == 0 then
            NodeG { value = hd, end = Just data, left = EmptyG, right = EmptyG, equal = EmptyG }
          else
            NodeG { value = hd, end = Nothing, left = EmptyG, right = EmptyG, equal = EmptyG }
            |> addEqualG (insertGHelp data tl EmptyG)
    NodeG value ->
      case chars of
        [] -> tst
        hd :: tl ->
          if hd == value.value then
            if List.length tl == 0 then
              NodeG { value | end = Just data }
            else
              NodeG { value | equal = insertGHelp data tl value.equal }
          else if hd < value.value then
            NodeG { value | left = insertGHelp data chars value.left }
          else
            NodeG { value | right = insertGHelp data chars value.right }

insertG : a -> TSTG a -> TSTG a
insertG data (TSTG hash inside) =
  TSTG hash (insertGHelp data (String.toList (hash data)) inside)

searchGHelp : List Char -> TSTGInside a -> Bool
searchGHelp chars tst =
  case tst of
    EmptyG -> False
    NodeG value ->
      case chars of
        [] -> False
        hd :: tl ->
          if value.value == hd then
            if List.length tl == 0 then
              value.end /= Nothing
            else
              searchGHelp tl value.equal
          else if hd < value.value then
            searchGHelp chars value.left
          else
            searchGHelp chars value.right

searchG : a -> TSTG a -> Bool
searchG data (TSTG hash inside) =
  searchGHelp (String.toList (hash data)) inside

isEmptyG : TSTGInside a -> Bool
isEmptyG tst =
  case tst of
    EmptyG -> True
    NodeG _ -> False

removeEmptyNodeG : TSTGInside a -> TSTGInside a
removeEmptyNodeG tst =
  case tst of
    EmptyG -> EmptyG
    NodeG { end, left, right, equal } ->
      if isEmptyG left && isEmptyG equal && isEmptyG right && end == Nothing then
        EmptyG
      else
        tst

deleteGHelp : List Char -> TSTGInside a -> TSTGInside a
deleteGHelp chars tst =
  case tst of
    EmptyG -> EmptyG
    NodeG value ->
      case chars of
        [] -> tst
        hd :: tl ->
          if value.value == hd then
            if List.length tl == 0 then
              if isEmptyG value.right && isEmptyG value.equal && isEmptyG value.left then
                EmptyG
              else
                NodeG { value | end = Nothing }
            else
              tst
              |> addEqualG (deleteGHelp tl value.equal)
              |> removeEmptyNodeG
          else if hd < value.value then
            NodeG { value | left = deleteGHelp chars value.left }
            |> removeEmptyNodeG
          else
            NodeG { value | right = deleteGHelp chars value.right }
            |> removeEmptyNodeG

deleteG : a -> TSTG a -> TSTG a
deleteG data (TSTG hash inside) =
  TSTG hash (deleteGHelp (String.toList (hash data)) inside)


foldlGHelp : (a -> b -> b) -> b -> TSTGInside a -> b
foldlGHelp fun acc tst =
  case tst of
    EmptyG -> acc
    NodeG value ->
      acc
      |> \acc1 -> foldlGHelp fun acc1 value.left
      |> (\acc2 ->
        case value.end of
          Nothing -> acc2
          Just data -> fun data acc)
      |> \acc3 -> foldlGHelp fun acc3 value.equal
      |> \acc4 -> foldlGHelp fun acc4 value.right

foldlG : (a -> b -> b) -> b -> TSTG a -> b
foldlG fun acc (TSTG hash inside) =
  foldlGHelp fun acc inside

main : Html.Html msg
main =
  empty
  |> insert "test"
  |> insert "tesa"
  -- |> insert "tet"
  -- |> insert "espace"
  -- |> values
  |> delete "test"
  |> Debug.toString
  |> Html.text
