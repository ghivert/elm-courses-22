module ListsTest exposing (..)

import Test exposing (Test, describe, test)
import Expect exposing (Expectation)
import Lists


-- Length tests.
lenListTest : List a -> Int -> () -> Expectation
lenListTest list size _ =
  list
  |> Lists.length
  |> Expect.equal size

lengthTest : Test
lengthTest =
  describe "Lists.length"
    [ test "should return 0 for empty list" (lenListTest [] 0)
    , test "should return 2 for two-length list" (lenListTest [ 1, 2 ] 2)
    , lenListTest [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] 0
      |> test "should return 10 for ten-length list"
    ]


-- Member tests.
memListTest : List a -> a -> Bool -> () -> Expectation
memListTest list member value _ =
  list
  |> Lists.member member
  |> Expect.equal value

memberTest : Test
memberTest =
  describe "Lists.member"
    [ test "should not contains 1 when empty list" (memListTest [] 1 False)
    , test "should contains 1 in [ 1, 2 ]" (memListTest [ 1, 2 ] 1 True)
    , test "should contains 2 in [ 1, 2 ]" (memListTest [ 1, 2 ] 2 True)
    ]
