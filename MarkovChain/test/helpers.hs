
module Main where

import General
import Test.Tasty
import Test.Tasty.HUnit
import System.Directory
import System.Random
import Data.Set(fromList,Set,toList)
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams(toFile)


main :: IO ()
main = do 
  defaultMain 
    (
      testGroup 
        "all" 
        [ 
          helperTests
          ,mbTargetStateTests
        ]
    )


helperTests = testGroup 
  "helperTests" 
  [
    testCase "lSum" (
      assertEqual 
        "" 
        [0.1,0.30000000000000004,0.6000000000000001,1.0]
        (lSum [0.1,0.2,0.3,0.4])
    )
    ,testCase "find interval" (
      do
        assertEqual
          "" 
          0
          (findIntervalFromLeft [0.1,0.3,0.6,1.0]  0.05) 
        assertEqual
          "" 
          1
          (findIntervalFromLeft [0.1,0.3,0.6,1.0]  0.15) 
        assertEqual
          "" 
          3
          (findIntervalFromLeft [0.1,0.3,0.6,1.0]  0.9) 
    )
    ,testCase "cAR1" (
      assertEqual 
        "count the 3s and return a list without the 3s" 
        (4,[2,2,1])
        (cAR1 3 ([3,1,3,2,3,2,3]))
    )
    ,testCase "cARRec" (
      assertEqual 
        "count all states and return an empty list " 
        ([(1,1),(2,2),(3,4)],[])
        (cARRec ([],[3,1,3,2,3,2,3]))
    )
    ,testCase "targetFrequencies" (
      assertEqual 
        "count all states" 
        [(1,1),(2,2),(3,4)]
        (targetFrequencies [3,1,3,2,3,2,3])
    )
    ,testCase "targetProbs" (
      assertEqual 
        "compute Probabilities" 
        [(1,1.0/7.0),(2,2.0/7.0),(3,4.0/7.0)]
        (targetProbs [(1,1),(2,2),(3,4)])
    )
    ]


mbTargetStateTests= testGroup 
  "Maybe targetStateTests" 
  [
    testCase "determine the target state from the present state and a number between 0 and 1"  (
      assertEqual 
        "since the prob for 2 is 1.0 the result has to be 2 regardless of the 'random' number 0.5" 
        (Just 2)
        (mbTargetState (Just [(2,1.0)]) 0.5)
    )
    ,testCase "determine the target state from the present state and a number between 0 and 1"  (
      assertEqual 
        "since the prob for 1 is 1/6 and 1/6 for 2 2/3 for 3 the result has to be 1 for 'random number 0.1<1/6"
        (Just 1)
        (mbTargetState (Just [(1,1.0/6.0),(2,1.0/6),(3,2.0/3.0)]) 0.1)
    )
    ,testCase "determine the target state from the present state and a number between 0 and 1"  (
      assertEqual 
        "since the prob for 1 is 1/6 and 1/6 for 2 2/3 for 3 the result has to be 2 for 'random number 1/6<0.2<(1/6+1/6)"
        (Just 2)
        (mbTargetState (Just [(1,1.0/6.0),(2,1.0/6),(3,2.0/3.0)]) 0.2)
    )
    ,testCase "determine the target state from the present state and a number between 0 and 1"  (
      assertEqual 
        "since the prob for 1 is 1/6 and 1/6 for 2 2/3 for 3 the result has to be 3 for 'random number 1/6+1/6 <0.4"
        (Just 3)
        (mbTargetState (Just [(1,1.0/6.0),(2,1.0/6),(3,2.0/3.0)]) 0.4)
    )
    ,testCase "targetState" (
      assertEqual 
        "in case we landed in a state without known successors we return Nothing" 
        Nothing
        (mbTargetState Nothing 0.1)
    )
  ]
