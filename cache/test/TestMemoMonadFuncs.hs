
module Main where

import Test.Tasty
import Test.Tasty.HUnit

import MemoMonadFuncs

main :: IO ()
main = do
  defaultMain (testGroup "Testing the original versions of the factorial function and the function computing the n-th fibonachi number" [ fibonacci_2])

--factorial :: TestTree
--factorial = testCase "Testing fac"
--  (assertEqual "Should say 2*3*4=24"  24 (fact 4))

--fibonacci_0:: TestTree
--fibonacci_0= testCase "Testing fibonacci"
--  (assertEqual "fib 0 should say 0."  0 (fib  0))
--
--fibonacci_1:: TestTree
--fibonacci_1= testCase "Testing fibonacci"
--  (assertEqual "fib 1 should say 1."  1 (fib  1))

fibonacci_2:: TestTree
fibonacci_2= testCase "Testing fibonacci"
  (assertEqual "fib 2 should say 0+1."  1 (evalFibmId 2))

