module Tests.Bits exposing (..)

import Test exposing (Test, describe, test)
import Expect exposing (Expectation)

-- That file tests should test all the functions in src/Bits.elm.
--   It's up to you to write all the tests. Two tests structures are given,
--   try to fix them, and then add some new tests.

dummyFstTest : () -> Expectation
dummyFstTest () = Expect.pass

dummySndTest : () -> Expectation
dummySndTest () = Expect.pass

dummyTest : Test
dummyTest =
  describe "Bits.bitToString"
    [ test "should return \"0\" for Zero" dummyFstTest
    , test "should return \"1\" for One" dummySndTest
    ]
