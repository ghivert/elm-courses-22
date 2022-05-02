module MathsTest exposing (..)

import Test exposing (Test, describe, test)
import Expect exposing (Expectation)
import Maths


-- Sum
sumTrue : a -> Expectation
sumTrue _ =
  25
  |> Maths.sum
  |> Expect.equal 325

sumFalse : a -> Expectation
sumFalse _ =
  20
  |> Maths.sum
  |> Expect.notEqual 200

sumTest : Test
sumTest =
  describe "Maths.sum"
    [ test "with 25 should return 325" sumTrue
    , test "with 20 should not return 200" sumFalse
    ]


-- Fact
factTrue : a -> Expectation
factTrue _ =
  5
  |> Maths.fact
  |> Expect.equal 120

factFalse : a -> Expectation
factFalse _ =
  10
  |> Maths.fact
  |> Expect.notEqual 3628800

factTest : Test
factTest =
  describe "Maths.fact"
    [ test "with 5 should return 120" factTrue
    , test "with 10 should not return 3628800" factFalse
    ]


-- Fibonacci
fibNotTail : Int -> Int -> a -> Expectation
fibNotTail value res _ =
  value
  |> Maths.fib
  |> Expect.equal res

fibTail : Int -> Int -> a -> Expectation
fibTail value res _ =
  value
  |> Maths.fibTail
  |> Expect.equal res

fibTest : Test
fibTest =
  describe "Maths.fib"
    [ test "with 7 should return 13" (fibNotTail 7 13)
    , test "with 15 should not return 610" (fibNotTail 15 610)
    ]

fibTailTest : Test
fibTailTest =
  describe "Maths.fibTail"
    [ test "with 7 should return 13" (fibTail 7 13)
    , test "with 15 should not return 610" (fibTail 15 610)
    ]
