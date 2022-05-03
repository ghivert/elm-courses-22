module Tests.Lists exposing (..)

import Test exposing (Test, describe, test)
import Expect exposing (Expectation)
import Lists
import Utils.Lists as Utils

-- That file tests all the functions in src/Lists.elm.
--   Fill in the blanks in source file and the tests will run for you.
--   Most tests should indicates an error at first. Don't take it personnally.

-- Length tests. Computes the length of a List without using List.length.
lengthTest : List a -> Int -> () -> Expectation
lengthTest list size () = Lists.length list |> Expect.equal size

length : Test
length =
  describe "Lists.length"
    [ test "should return 0 for empty list" (lengthTest [] 0)
    , test "should return 2 for two-length list" (lengthTest [ 1, 2 ] 2)
    , test "should return 10 for ten-length list"
        <| lengthTest [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] 0
    ]


-- Member tests. Checks if a value is includes in the List without using
--   List.member.
memberTest : List a -> a -> Bool -> () -> Expectation
memberTest list memb value () = Lists.member memb list |> Expect.equal value

member : Test
member =
  describe "Lists.member"
    [ test "should not contains 1 when empty list" (memberTest [] 1 False)
    , test "should contains 1 in [ 1, 2 ]" (memberTest [ 1, 2 ] 1 True)
    , test "should contains 2 in [ 1, 2 ]" (memberTest [ 1, 2 ] 2 True)
    ]


-- Filter Even. Keeps all elements at an even position in the List.
filterEven : Test
filterEven =
  describe "Lists.filterEven"
    [ test "should return [] for []" (Utils.filter Lists.filterEven [] [])
    , test "should return [ 3, 6 ] for [ 2, 3, 4, 6 ]"
        <| Utils.filter Lists.filterEven [ 2, 3, 4, 6 ] [ 3, 6 ]
    ]


-- Filter. Removes all elements not satistfying the predicate.
filter : Test
filter =
  let example = [ 1, 2, 3 ] in
  describe "Lists.filter"
    [ test "should return [] for []"
        <| Utils.filter (Lists.filter (always True)) [] []
    , test "should return [ 1, 2, 3 ] for (always True) and [ 1, 2, 3 ]"
        <| Utils.filter (Lists.filter (always True)) example example
    , test "should return [ 1 ] for (\\a -> a <= 1) and [ 1, 2, 3 ]"
        <| Utils.filter (Lists.filter (\a -> a <= 1)) example [ 1 ]
    ]


-- Sum. Computes the sum of a list of Int.
sumEmptyTest : () -> Expectation
sumEmptyTest () = Lists.sum [] |> Expect.equal 0

sumTest : () -> Expectation
sumTest () = Lists.sum [ 1, 2, 6, 8 ] |> Expect.equal 17

sumHugeTest : () -> Expectation
sumHugeTest () = Lists.sum [ 2, 5, 98, 63, 65, 456 ] |> Expect.equal 689

sum : Test
sum =
  describe "Lists.sum"
    [ test "should return 0 for []" sumEmptyTest
    , test "should return 17 for [ 1, 2, 6, 8 ]" sumTest
    , test "should return 689 for [ 2, 5, 98, 63, 65, 456 ]" sumHugeTest
    ]


-- Append takes two Lists and append the second to the first without
--   using List.append.
appendEmptyTest : () -> Expectation
appendEmptyTest () = Lists.append [] [] |> Expect.equal []

appendLeftTest : () -> Expectation
appendLeftTest () = Lists.append [ 1, 2 ] [] |> Expect.equal [ 1, 2 ]

appendRightTest : () -> Expectation
appendRightTest () = Lists.append [] [ 3, 4 ] |> Expect.equal [ 3, 4 ]

appendFstTest : () -> Expectation
appendFstTest () = Lists.append [ 1, 2 ] [ 3, 4 ] |> Expect.equal [ 1, 2, 3, 4 ]

appendSndTest : () -> Expectation
appendSndTest () = Lists.append [ 3, 4 ] [ 1, 2 ] |> Expect.equal [ 3, 4, 1, 2 ]

append : Test
append =
  describe "Lists.append"
    [ test "should return [] for [] and []" appendEmptyTest
    , test "should return [ 1, 2 ] for [ 1, 2 ] and []" appendLeftTest
    , test "should return [ 3, 4 ] for [] and [ 3, 4 ]" appendRightTest
    , test "should return [ 1, 2, 3, 4 ] for [ 1, 2 ] and [ 3, 4 ]" appendFstTest
    , test "should return [ 3, 4, 1, 2 ] for [ 3, 4 ] and [ 1, 2 ]" appendSndTest
    ]
