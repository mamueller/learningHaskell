module Main where

import Lib
import Test.Tasty
import Test.Tasty.HUnit
import System.Random
import Data.Set(fromList,Set,toList)
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams(toFile)
import System.Directory

main :: IO ()
main = do 
  defaultMain 
    ( 
      testGroup 
        "plot" 
        [
          plotTrajectoryTests
          ,
          discretizeTests 
        ]
    )

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
      , testCase "center" ( assertEqual "True" (x2ind 0.0) (5)) 
      , testCase "roundTrips" 
        ( 
          assertEqual 
          "i->x->i" 
          [ x2ind (ind2x ind) | ind <- ss] 
          ss
        ) 
      , testCase 
          "plot" 
          (
            let
              nos = 40
              (x2ind,ind2x) = (discretize x_min x_max nos) 
              nt=2^5
              generator = mkStdGen 12 
              rnums = take nt ( randomRs (0.0,1.0) generator ::[Double])
              
              times=[fromIntegral ind| ind <- [1..nt]] ::[Double]
              values = [ sin (t*3.14159/fromIntegral nt)   | t<- times]::[Double]
              recorded_points = zip times values
              recorded_indices= [ ((x2ind v))| v<- values]
              
              discretized_points = [(t,ind2x (x2ind v))| (t,v)<- recorded_points]::[(Double,Double)]

              phi = phiTwo_maker recorded_indices
              s0:s1:rest=recorded_indices 
              reproduced_indices = (ind_traj_2 (s0,s1) phi rnums)
            in
              do
                toFile def "signalVsDicretisation.svg" $ do
                  layout_title .= "Signal vs.Discretization "
                  setColors [opaque blue, opaque red]
                  mapM_ (\ (label,points) -> plot(line label [points])) 
                        [ 
                          ("org",recorded_points)
                        , ("disc",discretized_points)
                        ]
                print "########## Test discretization##########" 
                toFile def "indexSignalvsindtraj.svg" $ do
                  layout_title .= "Signal indices vs ind_traj2 "
                  setColors [opaque blue, opaque red]
                  mapM_ (\ (label,points) -> plot(line label [points])) 
                        [ 
                          ("org",(zip times (map fromIntegral recorded_indices)::[(Double,Double)]))
                        , ("disc",(zip times (map fromIntegral reproduced_indices)::[(Double,Double)]))
                        ]
            )

        
    ]

plotTrajectoryTests = let
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
      testCase "svg " (
        let 
          fn ="test.svg"
          generator = mkStdGen 12
        in
            --generator <- getStdGen
          toFile def "alltrajectories.svg" $ do
            layout_title .= "original vs first order vs second order"
            setColors [opaque blue, opaque red, opaque green]
            mapM_ (\ (label,points) -> plot(line label [points])) 
              [
                ("recorded",recorded_points) 
                ,("first order mc",(reproduced_points recorded_points nos generator)) 
                ,("second order mc",(reproduced_points_2 recorded_points nos generator))
              ]
        )
    ]
