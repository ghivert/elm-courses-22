module Tests.Strings exposing (..)

import Test exposing (Test, describe, test)
import Expect exposing (Expectation)
import Strings

-- That file tests all the functions in src/Strings.elm.
--   Fill in the blanks in source file and the tests will run for you.
--   Most tests should indicates an error at first. Don't take it personnally.

-- Palyndromes checks if a word can be read in both way (left to right and
--   right to left).
lavalTrue : () -> Expectation
lavalTrue () =
  "laval"
  |> Strings.isPalyndrome
  |> Expect.true "laval is a palyndrome"

azertyFalse : () -> Expectation
azertyFalse () =
  "azerty"
  |> Strings.isPalyndrome
  |> Expect.false "azerty is not a palyndrome"

isPalyndrome : Test
isPalyndrome =
  describe "Strings.isPalyndrome"
    [ test "should return true for laval" lavalTrue
    , test "should return false for azerty" azertyFalse
    ]


-- Pangrams checks if a word contains all the alphabet inside.
pangramTrue : () -> Expectation
pangramTrue () =
  "The quick brown fox jumps over the lazy dog"
  |> Strings.isPangram
  |> Expect.true "'The quick brown fox jumps over the lazy dog' is a pangram"

pangramFalse : () -> Expectation
pangramFalse () =
  "bloup"
  |> Strings.isPangram
  |> Expect.false "bloup is not a pangram"

isPangram : Test
isPangram =
  describe "Strings.isPangram"
    [ test "should return true for 'The quick brown fox jumps over the lazy dog'" pangramTrue
    , test "should return false for bloup" pangramFalse
    ]
