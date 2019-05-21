module Main where

import Test.Tasty
import Test.Tasty.HUnit

import FivePointFour
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
    defaultMain (testGroup "set Tests" [
         equalSetTest 
        ,uniqueElementlistTest 
        ,setEmptyTest_1
        ,setEmptyTest_2
        ,addSetTest
        ,delSetTest
        ,inSetTest
        ,notInSetTest
        ,includedTest
        ,notIncludedTest
        ,interTest
        ,emptyInterTest
        ,unionTest
        ])


equalSetTest:: TestTree
equalSetTest= testCase "Testing equality of sets constructed from lists with duplicates "
    (assertEqual "Should return True"  (setfromList [p1,p1,p2]) (setfromList [p1,p2]))

uniqueElementlistTest:: TestTree
uniqueElementlistTest= testCase "Testing uniqueness of elementslists of sets constructed from lists with duplicates "
    (assertEqual "Should return True"  (elementlist((addSet p1 (Set [p1,p2])))) (elementlist(Set [p1,p2])))

setEmptyTest_1:: TestTree
setEmptyTest_1= testCase "Testing empty set "
    (assertEqual "Should return True" True ( setEmpty (Set [])))

setEmptyTest_2:: TestTree
setEmptyTest_2= testCase "Testing one element set "
    (assertEqual "Should return False " False ( setEmpty (Set [p1] )))

addSetTest::TestTree
addSetTest = testCase "testing that an element is inserted at the right position"
    (assertEqual "should return " (Set [p1,p2,p3]) (addSet p2 (Set [p1,p3] ))) 

delSetTest::TestTree
delSetTest = testCase "testing that an element is deleted "
    (assertEqual "should return " [p2,p3] (elementlist (delSet p1 (Set [p1,p2,p3] )))) 

inSetTest::TestTree
inSetTest = testCase "testing that in set"
    (assertEqual "should return True" True (inSet (Set [p1,p2]) p1))

notInSetTest::TestTree
notInSetTest = testCase "testing that in set"
    (assertEqual "should return False" False (inSet (Set [p1,p2]) p3))

includedTest::TestTree
includedTest = testCase "testing that set a is in set b"
    (assertEqual "should return True" True (included (Set [p1,p2]) (Set [p1,p2,p3])))

notIncludedTest::TestTree
notIncludedTest = testCase "testing that set a is not in set b"
    (assertEqual "should return False" False (included (Set [p1,p2,p3]) (Set [p1,p2])))

interTest::TestTree
interTest = testCase "testing the intersection of two sets"
    (assertEqual "should return " (Set [p1,p2]) (inter (Set [p1,p2]) (Set [p1,p2,p3])))

emptyInterTest::TestTree
emptyInterTest = testCase "testing the intersection of two disjunct sets"
    (assertEqual "should return " (Set []) (inter (Set [p1,p2]) (Set [p3])))

unionTest::TestTree
unionTest = testCase "testing the intersection of two disjunct sets"
    (assertEqual "should return " (Set [p1,p2,p3]) (union (Set [p1,p2]) (Set [p1,p3])))

