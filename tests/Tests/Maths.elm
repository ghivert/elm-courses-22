module Tests.Maths exposing (..)

import Test exposing (Test, describe, test)
import Expect exposing (Expectation)
import Maths

-- That file tests all the functions in src/Maths.elm.
--   Fill in the blanks in source file and the tests will run for you.
--   Most tests should indicates an error at first. Don't take it personnally.

-- Sum adds all the number from 1 to n.
sumTrue : () -> Expectation
sumTrue () = Maths.sum 25 |> Expect.equal 325

sumFalse : () -> Expectation
sumFalse () = Maths.sum 20 |> Expect.notEqual 200

sumTest : Test
sumTest =
  describe "Maths.sum"
    [ test "with 25 should return 325" sumTrue
    , test "with 20 should not return 200" sumFalse
    ]


-- Fact multiplies all the numbers from 1 to n.
factTrue : () -> Expectation
factTrue () = Maths.fact 5 |> Expect.equal 120

factFalse : () -> Expectation
factFalse () = Maths.fact 10 |> Expect.equal 3628800

factTest : Test
factTest =
  describe "Maths.fact"
    [ test "with 5 should return 120" factTrue
    , test "with 10 should return 3628800" factFalse
    ]


-- Fibonacci computes the nth Fibonacci number, tail rec and not tail rec.
fibNotTail : Int -> Int -> () -> Expectation
fibNotTail value res () = Maths.fib value |> Expect.equal res

fibTail : Int -> Int -> () -> Expectation
fibTail value res () = Maths.fibTail value |> Expect.equal res

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
