module Main where

import Test.Tasty
import Test.Tasty.HUnit

import FivePointTwo
import Point

p1 :: Point
p1 = Point{x=1, y=1}
p2 :: Point 
p2 = Point {x=2, y=2}
p3 :: Point 
p3 = Point {x=3, y=2}

myCompFunc :: Point->Point->Bool
myCompFunc p1 p2 = (dist p1) <= (dist p2)

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
    (assertEqual "Should return True" True ( pqEmpty (emptyPQ myCompFunc)))

pqEmptyTest_2:: TestTree
pqEmptyTest_2= testCase "Testing one element priority Queue"
    (assertEqual "Should return False " False ( pqEmpty (PQ [p1] myCompFunc)))

enPQTest::TestTree
enPQTest = testCase "testing that an element is inserted at the right position in the priority list"
    (assertEqual "should return " [p1,p2,p3] (elements (enPQ p2 (PQ [p1,p3] myCompFunc))) )

dePQTest::TestTree
dePQTest = testCase "testing that an element is deleted from the top of the the priority list"
    (assertEqual "should return " [p2,p3] (elements (dePQ (PQ [p1,p2,p3] myCompFunc)))) 

frontPQTest::TestTree
frontPQTest = testCase "testing that the element with smallest distance from the origin is at the head of  the priority queue"
    (assertEqual "should return p1" p1 ( frontPQ (PQ [p1] myCompFunc)))

