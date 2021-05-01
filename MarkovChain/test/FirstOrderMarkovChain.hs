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
  defaultMain (testGroup "all" [ phiMakerTests,  trajectoryTests])

trajectoryTests = let
  nos=5
  x_min = (-1.0)
  x_max = 1.0
  (x2ind,ind2x) = (discretize x_min x_max nos) 

  nt = 7
  ts :: [Double] 
  ts =[1..fromIntegral nt]

  recorded_points ::  [(Double,Double)]
  recorded_points = [ (t,(sin (t*3.14159/fromIntegral nt)  )) | t<- ts]

  recorded_state_ids ::  [Int]
  recorded_state_ids = [ (x2ind s) | (s,_) <- recorded_points]
  
  generator = mkStdGen 12 
  rnums = take nt ( randomRs (0.0,1.0) generator ::[Double])
  
  phi ::StateTransformer 
  phi ind rn 
      | ind < nos = ind+1
      | otherwise = nos
      -- the random rumber rn is completely ignored

  in testGroup
    "trajectory tests"
    [ 
      testCase "index trajectory" (
        assertEqual "" ([0,1,2,3,4,5,5])(ind_traj 0 phi rnums) )
    ]



phiMakerTests = testGroup "build completely predictable phi from recorded states" [phiMakerTestsOnSingleStates, phiMakerTestsOnTrajectories] 
phiMakerTestsOnSingleStates= testGroup 
  "learn phi and test it on "
  (
  let
    rn=0.5
    recorded=[0,1,2,3,4]
    defaultState=0
    phi = (phiMaker recorded defaultState)
    -- the random rumbers rn should not be
    -- relavent, since for the following test records phi is 
    -- completely deterministic 
    -- (There are never two or more target states)
  in
    [
      testCase "learn phi" (
          assertEqual "" 2 (phi 1 rn) 
      )
      ,testCase "learn phi" (
          assertEqual "" 3 (phi 2 rn) 
      )
      ,testCase "learn phi" (
          assertEqual "" 4 (phi 3 rn) 
      )
      ,testCase "learn phi" (
          assertEqual "" defaultState (phi 4 rn) 
      )
    ]
  )

phiMakerTestsOnTrajectories= testGroup 
  "Try to learn complete index trajectories"
  (
  let 
      recorded=[0,1,2,3,4,5,6]
      nt=length(recorded)
      --we create a record that would be totally predictable by a first order markov chain
      defaultState=0
      phi = phiMaker recorded defaultState
      --we dont need random numbers here since the successor can be computed for 
      --every state except 6
      generator = mkStdGen 12
      rnums = take nt ( randomRs (0.0,1.0) generator ::[Double])
  in
    [
      testCase "index trajectory learned" (
          assertEqual "" recorded (ind_traj 0 phi rnums) 
      )
      ,testCase "index trajectory learned" (
          assertEqual "" [1,2,3,4,5,6,defaultState] (ind_traj 1 phi rnums) 
      )
    ]
  )
