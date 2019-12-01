module Main where

import Test.Tasty
import Test.Tasty.HUnit

import FibCache
import FibMonad
import FacCache
import Control.Monad.Memo
--import Lib
--import MathLib

main :: IO ()
main = do
  defaultMain (testGroup "Our Library Tests" [ sayYoTest ])-- , add5Test ])

sayYoTest :: TestTree
sayYoTest = testCase "Testing fac"
  (assertEqual "Should say 2*3" 6 (fac 3))


