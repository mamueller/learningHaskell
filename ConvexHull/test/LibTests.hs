--module Main where
module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Lib
import MathLib

p1 :: Point
p1 = CartVec{x=1, y=1}
p2 :: CartVec 
p2 = CartVec {x=2, y=2}
p3 :: CartVec 
p3 = CartVec {x=3, y=4}
p4 :: CartVec 
p4 = CartVec {x=4, y=4}
p5 :: Point
p5 = CartVec{x=4, y=1}

main :: IO ()
main = do
    defaultMain (testGroup "Our Library Tests" [
        sayYoTest
        ,add5Test
        ,leftmostLowestTest
        ,grahamPrepTest
        ])

sayYoTest :: TestTree
sayYoTest = testCase "Testing sayYo"
    (assertEqual "Should say Yo to Friend!" "yo Friend!" (sayYo "Friend"))
    
add5Test :: TestTree
add5Test = testCase "Testing add5"
    (assertEqual "Should add 5 to get 10" 10 (add5 5))

leftmostLowestTest :: TestTree
leftmostLowestTest = testCase "left most lowest"
    (assertEqual "from a given list of Points we want the lowerLeftmost and the remaining points ordered by their angle with the x axis" p1 (leftmostLowest [p1,p2,p3,p4,p5]))

grahamPrepTest :: TestTree
grahamPrepTest = testCase "Testing graham preperation step"
    (assertEqual "from a given list of Points we want the lowerLeftmost and the remaining points ordered by their angle with the x axis"  True True)
--        (p1,[p5,p4]) (grahamPrep [p1,p4,p5]))
