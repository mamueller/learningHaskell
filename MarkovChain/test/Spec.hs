module Main where

import Lib
import Test.Tasty
import Test.Tasty.HUnit
--import Test.Tasty.Golden (goldenVsString, findByExtension)
--import System.FilePath (takeBaseName, replaceExtension)
import System.Directory
import System.Random


main :: IO ()
main = do 
  defaultMain (testGroup "all" [trajectoryTests, discretizeTests, helperTests])


discretizeTests = let 
  nos = 10 
  ss = [1..nos]
  x_min = (-1.0)
  x_max = 1.0
  (x2ind,ind2x) = (discretize x_min x_max nos) 
  in 
    testGroup 
    "discretize tests" 
    [
        testCase "lower end" ( assertEqual "True" (x2ind x_min) 0)
      , testCase "upper end" ( assertEqual "True" (x2ind x_max) nos) 
      , testCase "center" ( assertEqual "True" (x2ind 0.0) 5) 
      , testCase "roundTrips" 
        ( 
          assertEqual 
          "i->x->i" 
          [ x2ind (ind2x ind) | ind <- ss] 
          ss
        )
    ]

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
      assertEqual
        "" 
        4
        (findIntervalFromLeft [0.1,0.30000000000000004,0.6000000000000001,1.0]  0.9) 
    )
  ]

trajectoryTests = let
  nos=5
  x_min = (-1.0)
  x_max = 1.0
  (x2ind,ind2x) = (discretize x_min x_max nos) 

  ts :: [Double] 
  ts =[0,(0.5)..400]

  recorded_points ::  [(Double,Double)]
  recorded_points = [ (t,(sin (t*3.14159/45)  )) | t<- ts]

  recorded_state_ids ::  [Int]
  recorded_state_ids = [ (x2ind s) | (s,_) <- recorded_points]

  phi = phi_maker recorded_state_ids

  in testGroup
    "trajectory tests"
    [ 
      testCase "svg " (
        let 
          fn ="mychart.svg"
        in
          do
            a <-doesFileExist fn
            if a
              then removeFile fn
            else
              putStr "file not present"

            putStr "always"
            generator <- getStdGen
            make_svg recorded_points (reproduced_points recorded_points nos generator) fn
            b <- doesFileExist fn
            assertBool "exists" b
        )

      ,testCase "index trajectory" (
        assertEqual "" ([4]++[5,6,7,8])(ind_traj 4 phi nos) )
    ]
