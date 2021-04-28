
module Main where

import Lib
import Test.Tasty
import Test.Tasty.HUnit
import System.Random

main :: IO ()
main = do 
  defaultMain (testGroup "all" [twoHelperTests, ind_traj_2_Tests , phiTwo_makerTests])

twoHelperTests = testGroup 
  "tupleHelperTests" 
  [
    testCase "tupleList" (
      assertEqual 
        "" 
        [(1,2),(2,3)]
        (tupleList [1,2,3])
    )
    ,testCase "Successors of (1,2)" (
      assertEqual 
        "" 
        [3,3]
        (findTupleSuccessors (1,2) [1,2,3,2,1,2,3])
    )
    ,testCase "Successors of (2,3)" (
      assertEqual 
        "" 
        [2]
        (findTupleSuccessors (2,3) [1,2,3,2,1,2,3])
    )
    ,testCase "allTupleTargetProbs" (
      assertEqual 
        "compute Probabilities " 
        [
          ((1,2),[(3,1.0)])
          ,((2,3),[(2,1.0)])
          ,((3,2),[(1,1.0)])
          ,((2,1),[(2,1.0)])
        ]
        (allTupleTargetProbs [(1,2),(2,3),(3,2),(2,1)] [1,2,3,2,1,2,3] )
    )
    ,testCase "successful tupleLookUp" (
      assertEqual 
        "known base state" 
        [(2,1.0)]
        (tupleLookUp 
          [
            ((1,2),[(3,1.0)])
            ,((2,3),[(2,1.0)])
            ,((3,2),[(1,1.0)])
            ,((2,1),[(2,1.0)])
          ]
          (2,1)
        )
    )
    ,testCase "unsuccessful tupleLookUp" (
      assertEqual 
        "unknown base state" 
        []
        (tupleLookUp 
          [
            ((1,2),[(3,1.0)])
            ,((2,3),[(2,1.0)])
            ,((3,2),[(1,1.0)])
            ,((2,1),[(2,1.0)])
          ]
          (1,1) -- not in the list
        )
    )
    ,testCase "successful lookup" (
      assertEqual 
        "known base state" 
        (Just [(2,1.0)])
        (lookup 
          (2,1)
          [
            ((1,2),[(3,1.0)])
            ,((2,3),[(2,1.0)])
            ,((3,2),[(1,1.0)])
            ,((2,1),[(2,1.0)])
          ]
        )
    )
    ,testCase "un successful lookup" (
      assertEqual 
        "known base state" 
        Nothing
        (lookup 
          (1,1)-- not in the list
          [
            ((1,2),[(3,1.0)])
            ,((2,3),[(2,1.0)])
            ,((3,2),[(1,1.0)])
            ,((2,1),[(2,1.0)])
          ]
        )
    )
    ,testCase "tupleCondJumpProbs" (
      assertEqual 
        "compute Probabilities of successors of a tuple" 
        ((1,2),[(2,0.5),(3,0.5)])
        (tupleCondJumpProbs [1,2,3,2,1,2,2] (1,2) )
    )
  ]
ind_traj_2_Tests = testGroup 
  "test trajectories of a second order markov chain with completely predictable state transition operators"
  (
  let 
    reverse_recorded=[6,5,4,3,2,1,0]
    revtup_0=(1,0) --these are the right most elements of the recorded states 
    nt=length(reverse_recorded)
    -- the random rumbers rnums are completely ignored but have to be present
    -- as argument
    generator = mkStdGen 12 
    rnums = take nt ( randomRs (0.0,1.0) generator ::[Double])
    
    -- make up a completely predictable phi
    rev_phi ::TwoStateTransformer 
    rev_phi (c,p) rn = (c+1)
  in
    [
      testCase "We first test the internal helper function which reconstruct the index trajectory from right to left by a helper function" (
        assertEqual "" reverse_recorded (ind_traj_reverse_2 revtup_0 rev_phi rnums) 
      )
      ,testCase "Test index trajectory reconstructed from left to right" (
        let
          --recorded=[0,1,2,3,4,5,6]
          recorded= reverse reverse_recorded
          
          --exchange order of args since we now reconstruct from left to right
          phi ::TwoStateTransformer 
          phi (c,p) = rev_phi (p,c) 

          --these are the left most elements of the recorded states 
          --tup_0=(1,0) 
          (a,b)=revtup_0
          tup_0=(b,a)
        in
          assertEqual "" recorded (ind_traj_2 tup_0 phi rnums) 
      )
    ]
  )

phiTwo_makerTests = testGroup "build completely predictable phi from recorded states" [phiTwo_makerTestsOnTuples,phiTwo_makerTestsOnTrajectories] 
phiTwo_makerTestsOnTuples = testGroup 
  "learn phi and test it on "
  (
  let
    rn=0.5
    recorded=[0,1,2,3,4,5,6]
    phi = phiTwo_maker recorded
    -- the random rumbers rn should not be
    -- relavent, since for the following test records phi is 
    -- completely deterministic 
    -- (There are never two or more target states)
  in
    [
      testCase "learn phi" (
          assertEqual "" 2 (phi (0,1) rn) 
      )
      ,testCase "learn phi" (
          assertEqual "" 3 (phi (1,2) rn) 
      )
      ,testCase "learn phi" (
          assertEqual "" 4 (phi (2,3) rn) 
      )
    ]
  )
phiTwo_makerTestsOnTrajectories= testGroup 
  "Try to learn complete index trajectories"
  (
  let generator = mkStdGen 12
  in
    [
      testCase "index trajectory learned" (
        let
          recorded=[0,1,0,-1,0,1,0,-1,0]
          nt=length(recorded)
          rnums = take nt ( randomRs (0.0,1.0) generator ::[Double])
          phi = phiTwo_maker recorded
        
        in
          assertEqual "" recorded (ind_traj_2 (0,1) phi rnums) 
      )
      ,testCase "index trajectory learned" (
        let
          recorded=[0,1,2,1,0,-1,-2,-1,0,1,2,1,0,-1,-2,-1]
          nt=length(recorded)
          rnums = take nt ( randomRs (0.0,1.0) generator ::[Double])
          phi = phiTwo_maker recorded
        
        in
          assertEqual "" recorded (ind_traj_2 (0,1) phi rnums) 
      )
      ,testCase "trajectory learned" (
        let
          recorded=[0,1,2,3,2,1,0,-1,-2,-3,-2,-1,0,1,2,3,2,1,0,-1,-2,-3]
          nt=length(recorded)
          tup_0 = (recorded!!0,recorded!!1)
          rnums = take nt ( randomRs (0.0,1.0) generator ::[Double])
          phi = phiTwo_maker recorded
        in
          assertEqual "" recorded (ind_traj_2 tup_0 phi rnums) 
      )
    ]
  )
