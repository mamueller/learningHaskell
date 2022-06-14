module Main where
import Test.Tasty
import Test.Tasty.HUnit
--import Data.Array
import Lib

main :: IO ()
--main = putStrLn "Test suite not yet implemented"
main = do
    defaultMain (testGroup "lib tests" [
        rejectTest
        , acceptTest
        ])   

rejectTest :: TestTree
rejectTest = testCase "duplicates in the list" 
    (assertEqual "should be False" 
        False (reject [1,2])
    )

acceptTest :: TestTree
acceptTest = testCase "only accept if lenght is 3" 
    (assertEqual "should be False" 
        False (accept[1,2])
    )
