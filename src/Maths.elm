module Maths exposing (..)

{-| Exemples de récursion terminale et non-terminale.
La récursion terminale permet d'éviter les changements de contexte,
et évite d'accumuler de la mémoire sur la pile.
Il faut réfléchir en terme d'accumulateurs et identifier les calculs
nécessaires aux résultats pour les déplacer dans les accumulateurs. -}

-- Add all the number from 1 to n.
-- Change function body.
sum : Int -> Int
sum range =
  if range == 0 then
    range
  else
    range + sum (range - 1)

-- Récursion terminale pour sum.
termSum : Int -> Int -> Int
termSum acc range =
  if range == 0 then
    acc
  else
    let newAcc = acc + range
        newRange = range - 1 in
    termSum newAcc newRange

-- Helper pour s'assurer que la fonction sum est appelé correctement.
sum : Int -> Int
sum range = termSum 0 range

-- Multiply all the numbers from 1 to n.
-- Change function body.
fact : Int -> Int
fact n =
  if n == 1 then
    n
  else n * (fact (n - 1))

factTail : Int -> Int
factTail =
  let
    factTailHelp : Int -> Int -> Int
    factTailHelp acc n =
      if n == 0 then
        acc
      else
        factTailHelp (acc * n) (n - 1)
  in
    factTailHelp 0

-- Compute the nth Fibonacci number.
-- Change function body.
-- Récursion non-terminale pour fibonacci.
fibonacci : Int -> Int
fibonacci n =
  if n == 0 || n == 1 then
    n
  else
    let prev = fibonacci (n - 1)
        otherPrev = fibonacci (n - 2) in
    prev + otherPrev

-- Récursion terminale pour Fibonacci.
termFibonacci : Int -> Int -> Int -> Int
termFibonacci acc1 acc2 n =
  if n == 1 then
    acc2
  else
    termFibonacci acc2 (acc1 + acc2) (n - 1)

-- Compute the nth Fibonacci number in a tail-rec way.
-- Change function body.
fibTail : Int -> Int
fibTail n =
  if n == 0 || n == 1 then
    n
  else
    termFibonacci 0 1 n
