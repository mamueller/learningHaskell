--module Main where
module Main where

import Test.Tasty
import Test.Tasty.HUnit

import FivePointOne

p1 :: Point
p1 = Point{x=1, y=1}
p2 :: Point 
p2 = Point {x=2, y=2}
p3 :: Point 
p3 = Point {x=3, y=4}
p4 :: Point 
p4 = Point {x=4, y=4}
p5 :: Point
p5 = Point{x=4, y=1}

main :: IO ()
main = do
    defaultMain (testGroup "Our Library Tests" [
         pqEmptyTest_1
        ,pqEmptyTest_2
        ,enPQTest
        ,dePQTest
        ,frontPQTest
        ])

pqEmptyTest_1:: TestTree
pqEmptyTest_1= testCase "Testing empty priority Queue"
    (assertEqual "Should return True" True ( pqEmpty emptyPQ))

pqEmptyTest_2:: TestTree
pqEmptyTest_2= testCase "Testing one element priority Queue"
    (assertEqual "Should return False " False ( pqEmpty (enPQ p1 emptyPQ)))

enPQTest::TestTree
enPQTest = testCase "testing that an element is inserted at the right position in the priority list"
    (assertEqual "should return " p1 (frontPQ(enPQ p2 (enPQ p1 emptyPQ))))

dePQTest::TestTree
dePQTest = testCase "testing that an element is inserted at the right position in the priority list"
    (assertEqual "should return " (enPQ p2 emptyPQ) (dePQ(enPQ p2 (enPQ p1 emptyPQ))))

frontPQTest::TestTree
frontPQTest = testCase "testing that the element with smallest distance from the origin is at the head of  the priority queue"
    (assertEqual "should return p1" p1 ( frontPQ (enPQ p1 emptyPQ)))


