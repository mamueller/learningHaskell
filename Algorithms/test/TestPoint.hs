module Main where

import Test.Tasty
import Test.Tasty.HUnit

--import FivePointTwo
import Point


main :: IO ()
main = do
    defaultMain (testGroup "Our Library Tests" [
         constructorTest
        ])


constructorTest:: TestTree
constructorTest= testCase "Testing empty priority Queue"
    (assertBool "funny way to make sure that the value constructor is actually executed" (not( (Point 1 1) == (Point 1 2)) ))


