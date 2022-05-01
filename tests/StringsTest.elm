module StringsTest exposing (..)

import Test exposing (Test, describe, test)
import Expect exposing (Expectation)
import Strings

-- Palyndromes
lavalTrue : a -> Expectation
lavalTrue _ =
  "laval"
  |> Strings.isPalyndrome
  |> Expect.true "laval is a palyndrome"

azertyFalse : a -> Expectation
azertyFalse _ =
  "azerty"
  |> Strings.isPalyndrome
  |> Expect.false "azerty is not a palyndrome"

isPalyndrome : Test
isPalyndrome =
  describe "Strings.isPalyndrome"
    [ test "should return true for laval" lavalTrue
    , test "should return false for azerty" azertyFalse
    ]

-- Pangrams
pangramTrue : a -> Expectation
pangramTrue _ =
  "The quick brown fox jumps over the lazy dog"
  |> Strings.isPangram
  |> Expect.true "'The quick brown fox jumps over the lazy dog' is a pangram"

pangramFalse : a -> Expectation
pangramFalse _ =
  "bloup"
  |> Strings.isPangram
  |> Expect.false "bloup is not a pangram"

isPangram : Test
isPangram =
  describe "Strings.isPangram"
    [ test "should return true for 'The quick brown fox jumps over the lazy dog'" pangramTrue
    , test "should return false for bloup" pangramFalse
    ]
