
module Main where

import Lib
import Test.Tasty
import Test.Tasty.HUnit
--import Test.Tasty.Golden (goldenVsString, findByExtension)
--import System.FilePath (takeBaseName, replaceExtension)
import System.Directory
import System.Random
import Data.Set(fromList,Set,toList)
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams(toFile)


main :: IO ()
main = do 
  defaultMain (testGroup "all" [ helperTests, targetStateTests, mbTargetStateTests])


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
    ,testCase "Successors of 1" (
      assertEqual 
        "" 
        [2,2]
        (findSuccessors 1 [1,2,3,2,1,2,3])
    )
    ,testCase "Successors of 2" (
      assertEqual 
        "" 
        [3,1,3]
        (findSuccessors 2 [1,2,3,2,1,2,3])
    )
    ,testCase "targetLists" (
      assertEqual 
        "" 
        [
          (1,[2,2])
          ,(2,[3,1,3])
          ,(3,[2])
        ]
        (targetLists (fromList [1,2,3]) [1,2,3,2,1,2,3])
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
    ,testCase "mbCondJumpProbs" (
      assertEqual 
        "compute Probabilities the successors of in the given trajectory are [3,1,3]" 
        (2, Just [(1,1.0/3.0),(3,2.0/3.0)])
        (mbCondJumpProbs [1,2,3,2,1,2,3] 2)
    )
    ,testCase "mbCondJumpProbs" (
      assertEqual 
        "compute Probabilities the successors of in the given trajectory are [3,1,3]" 
        (4,Nothing)
        (mbCondJumpProbs [1,2,3,2,1,2,3] 4)
    )
    ,testCase "allTargetProbs" (
      assertEqual 
        "compute all Probabilities note that there is no entry for probabilities under the condition that we are in state 4 because it has no successors in the recorded indexlist" 
        [
          (1,[(2,1.0)])
          ,(2,[(1,1.0/3.0),(3,2.0/3.0)])
          ,(3,[(2,1.0)])
        ]
        (allTargetProbs [4,3,2,1] [1,2,3,2,1,2,3] )
    )
    ]

targetStateTests= testGroup 
  "targetStateTests" 
  [
    testCase "determine the target state from the present state and a number between 0 and 1"  (
      assertEqual 
        "since the prob for 2 is 1.0 the result has to be 2 regardless of the 'random' number 0.5" 
        2
        (targetState [(2,1.0)] 0.5)
    )
    ,testCase "determine the target state from the present state and a number between 0 and 1"  (
      assertEqual 
        "since the prob for 1 is 1/6 and 1/6 for 2 2/3 for 3 the result has to be 1 for 'random number 0.1<1/6"
        1
        (targetState [(1,1.0/6.0),(2,1.0/6),(3,2.0/3.0)] 0.1)
    )
    ,testCase "determine the target state from the present state and a number between 0 and 1"  (
      assertEqual 
        "since the prob for 1 is 1/6 and 1/6 for 2 2/3 for 3 the result has to be 2 for 'random number 1/6<0.2<(1/6+1/6)"
        2
        (targetState [(1,1.0/6.0),(2,1.0/6),(3,2.0/3.0)] 0.2)
    )
    ,testCase "determine the target state from the present state and a number between 0 and 1"  (
      assertEqual 
        "since the prob for 1 is 1/6 and 1/6 for 2 2/3 for 3 the result has to be 3 for 'random number 1/6+1/6 <0.4"
        3
        (targetState [(1,1.0/6.0),(2,1.0/6),(3,2.0/3.0)] 0.4)
    )
    ,testCase "targetState" (
      assertEqual 
        "in case we landed in a state without known successors we return a default state 0" 
        0
        (targetState [] 0.1)
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
