
module Main where

import NOrder
import General
import Test.Tasty
import Test.Tasty.HUnit
import System.Random

main :: IO ()
main = do 
  defaultMain 
    (
      testGroup 
        "all" 
        [
          nHelperTests
          ,listIndTrajReverseTests
          ,listPhiMakerTests
        ]
    )
    

nHelperTests = testGroup 
  "nHelperTests" 
  [ 
    nlistTests
    ,listSuccessorTests
    ,mbListCondJumpProbTests 
    ,mbSimplifiedListCondJumpProbTests 
  ]

nlistTests=testGroup
  "nListTests"
  [
    testCase "nList" (
      assertEqual 
        "" 
        ([[1,2]])
        (nList [1,2] 2)
    )
    ,testCase "nList" (
      assertEqual 
        "" 
        ([[1,2],[2,3]])
        (nList [1,2,3] 2)
    )
    ,testCase "nList" (
      assertEqual 
        "" 
        ([])
        (nList [1] 2)
    )
    ,testCase "nList" (
      assertEqual 
        "" 
        ([])
        (nList [1,2] 3)
    )
    ,testCase "recList" (
      assertEqual 
        "" 
        ([2],[[1,2]])
        (recList ([1,2],[]) 2)
    )
    ,testCase "recList" (
      assertEqual 
        "" 
        ([3],[[2,3],[1,2]])
        (recList ([1,2,3],[]) 2)
    )
    ,testCase "oneList" (
      assertEqual 
        "" 
        (([2],[[1,2]]))
        (oneList ([1,2], []) 2)
    )
    ,
    testCase "oneList" (
      assertEqual 
        "" 
        (([2,3],[[1,2]]))
        (oneList ([1,2,3], []) 2)
    )
    ,testCase "oneList 2" (
      assertEqual 
        "" 
        (([3],[[2,3],[1,2]]))
        (oneList ([2,3], [[1,2]]) 2)
    )
    ,testCase "oneList 3" (
      assertEqual 
        "" 
        ([3], [[2,3],[1,2]])
        (oneList ([3], [[2,3],[1,2]]) 2)
    )
    ,testCase "oneList 4" (
      assertEqual 
        "" 
        ([], [[2,3],[1,2]])
        (oneList ([], [[2,3],[1,2]]) 2)
    )
  ]
    
    
listSuccessorTests=testGroup 
   "listSuccessorTests" 
   [
      testCase "add one Successors of [1,2]" (
        assertEqual 
          "" 
          ([2,3],[3,3])
          (recSuccessors [1,2] [1,2,3,2,1,2,3] [])
      )
      ,testCase "add one Successors of [1,2]" (
        assertEqual 
          "" 
          ([2,3,2],[2,2])
          (recSuccessors [1,2,3] [1,2,3,2,1,2,3,2] [])
      )
      ,testCase 
      "Successors of [1,2]" 
      (
        assertEqual 
          "" 
          ([3,3])
          (listSuccessors [1,2] [1,2,3,2,1,2,3])
      )
      ,testCase 
      "Successors of [1,2,3]"
      (
        assertEqual 
          "" 
          ([2,2])
          (listSuccessors [1,2,3] [1,2,3,2,1,2,3,2])
      )
      ,testCase 
      "Successors of [1,2,3,4]"
      (
        assertEqual 
          "" 
          ([])
          (listSuccessors [1,2,3,4] [1,2,3,2,1,2,3,2])
      )
   ]

allListTargetProbTests=testGroup
  "allListTargetProbTests"
  [
    testCase "allListTargetProbs" 
    (
      assertEqual 
        "compute Probabilities " 
        [
           ([1],[(3,1.0)])
          ,([2],[(3,2.0/3),(1,1.0/3)])
          ,([3],[(2,1.0)])
        ]
        (
          allListTargetProbs 
            [[1],[2],[3]] 
            [1,2,3,2,1,2,3]
        ) 
    )
    ,
    testCase "allListTargetProbs" 
    (
      assertEqual 
        "compute Probabilities " 
        [
           ([1,2],[(3,1.0)])
          ,([2,3],[(2,1.0)])
          ,([3,2],[(1,1.0)])
          ,([2,1],[(2,1.0)])
        ]
        (
          allListTargetProbs 
            [[1,2],[2,3],[3,2],[2,1]] 
            [1,2,3,2,1,2,3]
        ) 
    )
    ,
    testCase "un successful lookup" (
      assertEqual 
        "known base state" 
        Nothing
        (lookup 
          [1,1]-- not in the list
          [
             ([1,2],[(3,1.0)])
            ,([2,3],[(2,1.0)])
            ,([3,2],[(1,1.0)])
            ,([2,1],[(2,1.0)])
          ]
        )
    )
    ,
    testCase "successful lookup" (
      assertEqual 
        "known base state" 
        (Just [(2,1.0)])
        (lookup 
          [2,1]
          [
             ([1,2],[(3,1.0)])
            ,([2,3],[(2,1.0)])
            ,([3,2],[(1,1.0)])
            ,([2,1],[(2,1.0)])
          ]
        )
    )
  ]

mbListCondJumpProbTests=testGroup
  "mbListCondJumpProbTests"
  [
    testCase "mbListCondJumpProbs" (
      assertEqual 
        "compute Probabilities of successors of a list" 
        (Just [(1,1.0/3),(3,1.0/3),(2,1.0/3)])
        (mbListCondJumpProbs [1,2,3,2,1,2,2] [2] )
    )
    ,
    testCase "mbListCondJumpProbs" (
      assertEqual 
        "compute Probabilities of successors of a list" 
        (Just [(3,0.5),(2,0.5)])
        (mbListCondJumpProbs [1,2,3,2,1,2,2] [1,2] )
    )
    ,
    testCase "mbListCondJumpProbs" (
      assertEqual 
        "compute Probabilities of successors of a list" 
        (Just [(2,1)])
        (mbListCondJumpProbs [1,2,3,2,1,2,2] [1,2,3] )
    )
  ]

mbSimplifiedListCondJumpProbTests=testGroup
  "mbSimplifiedListCondJumpProbTests"
  [
    testCase "mbListCondJumpProbs" (
      assertEqual 
        "compute Probabilities of successors of a list" 
        --(Just [(2,0.75),(10,0.25)]) 10 is the outlier because it has a prob of 0.25 <0.3
        (Just [(2,1.0)])
        (mbSimplifiedListCondJumpProbs [1,2,1,2,1,2,1,10] [1] 0.3)
    )
    ,
    testCase "mbSimplifiedListCondJumpProbs" (
      assertEqual 
        "compute Probabilities of successors of a list" 
        (Just [(3,0.5),(2,0.5)])
        (mbSimplifiedListCondJumpProbs [1,2,3,2,1,2,2] [1,2] 0.1)
    )
    ,
    testCase "mbSimplifiedListCondJumpProbs" (
      assertEqual 
        "compute Probabilities of successors of a list" 
        (Just [(2,1)])
        (mbSimplifiedListCondJumpProbs [1,2,3,2,1,2,2] [1,2,3] 0.1)
    )
  ]

listIndTrajReverseTests= testGroup 
  "test trajectories of n order markov chains with completely predictable state transition operators"
  (
  let 
    reverse_recorded=[6,5,4,3,2,1,0]
    revListState_0=[1,0] --these are the right most elements of the recorded states 
    nt=length(reverse_recorded)-length(revListState_0)
    -- the random rumbers rnums are completely ignored but have to be present
    -- as argument
    generator = mkStdGen 12 
    rnums = take nt ( randomRs (0.0,1.0) generator ::[Double])
    
    -- make up a completely predictable phi
    rev_phi ::ListStateTransformer 
    rev_phi sl rn = (head sl)+1
  in
    [
      testCase "We first test the internal helper function which reconstruct the index trajectory from right to left by a helper function" (
        assertEqual "" reverse_recorded (listIndTrajReverse revListState_0 rev_phi rnums) 
      )
      ,testCase "Test index trajectory reconstructed from left to right" (
        let
          --recorded=[0,1,2,3,4,5,6]
          recorded= reverse reverse_recorded
          
          --exchange order of args since we now reconstruct from left to right
          phi ::ListStateTransformer 
          phi listState = rev_phi (reverse listState) 

          --these are the left most elements of the recorded states 
          --listState_0=[0,1]
          listState_0=(reverse revListState_0)
        in
          assertEqual "" recorded (listIndTraj listState_0 phi rnums) 
      )
    ]
  )

listPhiMakerTests = 
  testGroup 
    "build completely predictable phi from recorded states" 
    [
      testGroup 
        "learn phi and test it on "
        (
        let
          rn=0.5
          recorded=[0,1,2,3,4,5,6]
          phi = listPhiMaker 
            recorded --trajectory to learn from
            2 --order
            0 --default state to return if no successors are known
          -- the random rumbers rn should not be
          -- relavent, since for the following test records phi is 
          -- completely deterministic 
          -- (There are never two or more target states)
        in
          [
            testCase "learn phi" (
                assertEqual "" 2 (phi [0,1] rn) 
            )
            ,testCase "learn phi" (
                assertEqual "" 3 (phi [1,2] rn) 
            )
            ,testCase "learn phi" (
                assertEqual "" 4 (phi [2,3] rn) 
            )
          ]
        )
      ,
      testGroup 
        "Try to learn complete index trajectories"
        (
          let generator = mkStdGen 12
          in
            [
              testCase 
                "index trajectory learned" 
                (
                  let
                    recorded=[0,1,0,-1,0,1,0,-1,0]
                    order=2
                    nt=length(recorded)-order
                    rnums = take nt ( randomRs (0.0,1.0) generator ::[Double])
                    phi = listPhiMaker 
                      recorded
                      order
                      0
                  
                  in
                    (assertEqual "" recorded ( listIndTraj [0,1] phi rnums)) 
                 )
              ,testCase 
                "index trajectory learned" 
                (
                  let
                    recorded=[0,1,2,1,0,-1,-2,-1,0,1,2,1,0,-1,-2,-1]
                    order=2
                    nt=length(recorded)-order
                    rnums = take nt ( randomRs (0.0,1.0) generator ::[Double])
                    phi = listPhiMaker 
                      recorded
                      order
                      0
                  
                  in
                    assertEqual "" recorded (listIndTraj [0,1] phi rnums) 
                 )
              ,testCase 
                "trajectory learned" 
                (
                  let
                    recorded=[0,1,2,3,2,1,0,-1,-2,-3,-2,-1,0,1,2,3,2,1,0,-1,-2,-3]
                    order=2
                    nt=length(recorded)-order
                    listState_0 = [recorded!!0,recorded!!1]
                    rnums = take nt ( randomRs (0.0,1.0) generator ::[Double])
                    phi = listPhiMaker 
                      recorded
                      order
                      0
                  in
                    assertEqual "" recorded (listIndTraj listState_0 phi rnums) 
                )
            ]
          )
      ,
      testGroup 
        "oulier detection"
        (
          let generator = mkStdGen 12
          in
            [
              testCase
                "curtail jump probs"
                (
                  assertEqual 
                    "check if the states (1 and 4) with lower probability than threshold  have been removed"
                    (Just [(3,0.5),(2,0.5)])
                    (
                      (Just [(3,0.3),(2,0.3),(1,0.1),(4,0.1) ]) >>= (curtail 0.1) 
                    )
                )
              ,testCase
                "curtail jump probs leaving nothing"
                (
                  assertEqual 
                    "the target states are all less probable than the threshold that they are all removed"
                    Nothing
                    (
                      (Just [(3,0.3),(2,0.3),(1,0.1),(4,0.1) ]) >>= (curtail 0.3) 
                    )
                )
              ,testCase 
                "index trajectory learned" 
                (
                  let
                    sequence=[0,1,2,3,2,1,0,-1,-2,-3,-2,-1]
                    synthetic = foldl (\acc el -> el++acc) [] (replicate 30 sequence)
                    outliers =[(2,10),(3,-4)] --[(index,value),..]
                    f::Int->Int --f smuggels in the outliers at the appointed position
                    f ind =
                      let 
                        lu = lookup ind outliers
                      in 
                        if (lu == Nothing) then
                          (synthetic !! ind) 
                        else
                          let 
                            (Just val) = lu
                          in
                            val
                    
                    recorded = [(f ind)| ind <- [0..((length synthetic) -1)]]
                        
                    order=2
                    nt=length(recorded)-order
                    rnums = take nt ( randomRs (0.0,1.0) generator ::[Double])
                    -- we will only consider conditional jump probabilities that are bigger than threshold
                    -- discard the smaller ones and rescale the rest to sum up to one
                    threshold=0.1 
                    defaultState=0
                    phi = simplifiedListPhiMaker 
                      recorded
                      order
                      threshold
                      defaultState
                  
                  in 
                    do
                      --print synthetic
                      --print recorded
                      (assertEqual "" synthetic( listIndTraj [0,1] phi rnums)) 
                 )
            ]
        )
    ]
