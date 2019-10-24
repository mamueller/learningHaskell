
import Test.Tasty
import Test.Tasty.HUnit

import Lib

main :: IO ()
--main = putStrLn "Test suite not yet implemented"
main = do
    defaultMain (testGroup "Our Library Tests" [
        spellerTest
        ,spellerTest2
        ,spellerTest3
        ])

spellerTest :: TestTree
spellerTest = testCase "Testing speller"
    (assertEqual "should say: T is for This" "T is for This" (speller ["This"]))

spellerTest2 :: TestTree
spellerTest2 = testCase "Testing speller with two words"
    (assertEqual 
        "should add a comma and an 'and' " 
        "T is for This, and i is for is" (speller ["This","is"]))

spellerTest3 :: TestTree
spellerTest3 = testCase "Testing speller with three words"
    (assertEqual 
        "should add a comma between the first two sentences a comma and an 'and' between second and third " 
        "T is for This, i is for is, and a is for a" (speller ["This","is","a"]))
