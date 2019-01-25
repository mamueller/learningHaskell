module Main where

import Test.Tasty
import Test.Tasty.HUnit

import SortLib

main :: IO ()
main = do
    defaultMain (testGroup "Our Library Tests" [quickSortByTest])

compareDistanceTo2:: Double ->Double ->Ordering
compareDistanceTo2 x y = compare (dist2 x) (dist2 y) 
    where dist2 x = abs (2-x)

quickSortByTest ::TestTree
quickSortByTest =testCase "Testing quickSortBy"
    (assertEqual "Schould sort w.r.t. distance to 2" [2,2.9,1,3.9,0] (quickSortBy compareDistanceTo2[0,1,2,2.9,3.9]))
