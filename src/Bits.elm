module Bits exposing (..)

type Bit
  = Zero
  | One

type alias Bits =
  List Bit

bitToString : Bit -> String
bitToString bit =
  case bit of
    Zero -> "0"
    One -> "1"

bitsToString : Bits -> String
bitsToString = List.map bitToString >> String.join ""

toIndexedInt : Int -> Bit -> Int
toIndexedInt index bit =
  case bit of
    Zero -> 0
    One -> 2 ^ index

toIntHelp : Int -> List (Int, Bit) -> Int
toIntHelp acc bits =
  case bits of
    [] -> acc
    (index, value) :: tl -> toIntHelp (acc + (toIndexedInt index value)) tl

toInt : Bits -> Int
toInt bits =
  bits
  |> List.reverse
  |> List.indexedMap Tuple.pair
  |> toIntHelp 0

toIntNonRec : Bits -> Int
toIntNonRec bits =
  bits
  |> List.reverse
  |> List.indexedMap toIndexedInt
  |> List.foldl (+) 0

fromIntHelp : Bits -> Int -> Bits
fromIntHelp bits value =
  if value == 0 then
    bits
  else
    let remainder = modBy 2 value
        newValue = value // 2 in
      if remainder == 0 then
        fromIntHelp (Zero :: bits) newValue
      else
        fromIntHelp (One :: bits) newValue

fromInt : Int -> Bits
fromInt = fromIntHelp [] >> List.reverse

isBit : Char -> Bool
isBit elem = elem == '0' || elem == '1'

checkBitValidity : Char -> Bool -> Bool
checkBitValidity elem acc =
  if not acc then
    acc
  else
    isBit elem

validBitsString : String -> Maybe (List Char)
validBitsString str =
  let list = String.toList str
      isValid = List.foldl checkBitValidity True list in
    if isValid then
      Just list
    else
      Nothing

toBit : Char -> Bit
toBit value =
  if value == '0' then
    Zero
  else
    One

fromString : String -> Maybe Bits
fromString = validBitsString >> Maybe.map (List.map toBit)

bitNot : Bit -> Bit
bitNot bit =
  case bit of
    Zero -> One
    One -> Zero

bitsNot : Bits -> Bits
bitsNot = List.map bitNot

padWithZeroHelp : Bits -> Bits -> List (Bit, Bit)
padWithZeroHelp li1 li2 =
  case (li1, li2) of
    (hd1 :: tl1, hd2 :: tl2) -> (hd1, hd2) :: padWithZero tl1 tl2
    ([], hd :: tl) -> (Zero, hd) :: padWithZero [] tl
    (hd :: tl, []) -> (hd, Zero) :: padWithZero tl []
    ([], []) -> []

padWithZero : Bits -> Bits -> List (Bit, Bit)
padWithZero li1 li2 =
  List.reverse
    (padWithZeroHelp
      (List.reverse li1)
      (List.reverse li2)
    )

genericTreatment : (Bit -> Bit -> Bit) -> Bits -> Bits -> Bits
genericTreatment adder bits1 bits2 =
  let bits = padWithZero bits1 bits2 in
  List.map (\(first, second) -> adder first second) bits

xor : Bits -> Bits -> Bits
xor =
  genericTreatment
    (\bit1 bit2 ->
      case (bit1, bit2) of
        (Zero, Zero) -> Zero
        (Zero, One) -> One
        (One, Zero) -> One
        (One, One) -> Zero
    )

and : Bits -> Bits -> Bits
and =
  genericTreatment
    (\bit1 bit2 ->
      case (bit1, bit2) of
        (Zero, Zero) -> Zero
        (Zero, One) -> Zero
        (One, Zero) -> Zero
        (One, One) -> One
    )

addBits : Bits -> Bits -> Bits
addBits bits1 bits2 = fromInt (toInt bits1 + toInt bits2)

-- On écrirait en Haskell : fromString bits1 <$> addBits <*> fromString bits2
addStringBits : String -> String -> Maybe Bits
addStringBits bits1 bits2 =
  fromString bits1
  |> Maybe.map addBits
  |> maybeApply (fromString bits2)

-- On écrirait en Haskell : pure addBits <*> fromString bits1 <*> fromString bits2
addStringBitsWithoutMap : String -> String -> Maybe Bits
addStringBitsWithoutMap bits1 bits2 =
  Just (addBits)
  |> maybeApply (fromString bits1)
  |> maybeApply (fromString bits2)

addStringBitsMap : String -> String -> Maybe Bits
addStringBitsMap bits1 bits2 =
  Maybe.map2 addBits
    (fromString bits1)
    (fromString bits2)

-- Cette fonction est le foncteur applicatif.
maybeApply : Maybe a -> Maybe (a -> b) -> Maybe b
maybeApply = Maybe.map2 (|>)
