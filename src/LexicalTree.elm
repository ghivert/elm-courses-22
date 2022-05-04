module LexicalTree exposing (..)

import Dict exposing (Dict)

type LexicalTree =
  LexicalTree
    { definition : Maybe String
    , children : Dict Char LexicalTree
    }

empty : LexicalTree
empty =
  LexicalTree { definition = Nothing, children = Dict.empty }

addWordHelp : List Char -> String -> LexicalTree -> LexicalTree
addWordHelp chars definition (LexicalTree tree) =
  LexicalTree <|
    case chars of
      [] -> { tree | definition = Just definition }
      char :: others ->
        case Dict.get char tree.children of
          Nothing ->
            { tree
              | children =
                Dict.insert char
                  (addWordHelp others definition empty)
                  tree.children
            }
          Just child ->
            { tree
              | children =
                Dict.insert char
                  (addWordHelp others definition child)
                  tree.children
            }

addWord : String -> String -> LexicalTree -> LexicalTree
addWord word definition tree =
  if word == "" then
    tree
  else
    addWordHelp (String.toList word) definition tree

getWord : String -> LexicalTree -> Maybe String
getWord word (LexicalTree tree) =
  case String.toList word of
    char :: others ->
      case Dict.get char tree.children of
        Nothing -> Nothing
        Just child -> getWord (String.fromList others) child
    [] ->
      tree.definition

isPresent : String -> LexicalTree -> Bool
isPresent word tree =
  case getWord word tree of
    Nothing -> False
    Just _ -> True

updateHelp : List Char -> String -> LexicalTree -> LexicalTree
updateHelp chars definition (LexicalTree tree) =
  LexicalTree <|
    case chars of
      [] -> { tree | definition = Just definition }
      char :: others ->
        case Dict.get char tree.children of
          Nothing -> tree
          Just child ->
            { tree
              | children =
                Dict.insert char
                  (updateHelp others definition child)
                  tree.children
            }

update : String -> String -> LexicalTree -> LexicalTree
update word definition tree =
  if word == "" then
    tree
  else
    updateHelp (String.toList word) definition tree

allWordsHelp : String -> LexicalTree -> List String
allWordsHelp prev (LexicalTree tree) =
  Dict.toList tree.children
  |> List.concatMap
    (\(letter, child) -> allWordsHelp (prev ++ String.fromChar letter) child)
  |> (::) prev

allWords : LexicalTree -> List String
allWords = allWordsHelp ""

toString : LexicalTree -> String
toString = allWords >> String.join " "

length : LexicalTree -> Int
length = allWords >> List.length

fold : (String -> a -> a) -> a -> LexicalTree -> a
fold fun acc tree =
  tree
  |> allWords
  |> List.foldl fun acc

foldLength : LexicalTree -> Int
foldLength = fold (\_ prev -> prev + 1) 0
