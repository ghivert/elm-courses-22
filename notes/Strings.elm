module Strings exposing (..)

{-| Le but est de tester les bibliothèques de gestion des String et de
comprendre comment celles-ci sont implémentées. -}

-- Teste si une String est un palyndrome, i.e. si elle peut se lire dans
--   les deux sens.
isPalyndrome : String -> Bool
isPalyndrome possiblePal =
  let strLen = String.length possiblePal in
    if strLen == 0 || strLen == 1 then
      True
    else
      let firstChar = String.slice 0 1 possiblePal
          endChar = String.slice (strLen - 1) strLen possiblePal in
        if firstChar == endChar then
          isPalyndrome (String.slice 1 (strLen - 1) possiblePal)
        else
          False

alphabet : String
alphabet = "abcdefghijklmnopqrstuvwxyz"

-- Teste si une String est un pangramme, i.e. si il contient toutes
--   les lettres de l'alphabet.
isPangram : String -> Bool
isPangram possiblePan =
  let alphabetAsChars = String.toList alphabet
      possiblePanAsChars = String.toList (String.toLower possiblePan)
      isMemberOfPan char acc = if not acc then False else List.member char possiblePanAsChars in
  List.foldl isMemberOfPan True alphabetAsChars
