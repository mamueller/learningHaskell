module Main where

import Lib
import System.Directory

import Control.Monad  
import Data.Char  
import qualified Data.Set as S -- (fromList,Set,toList)
import System.Random 
import System.Directory
import Probability
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams(toFile)
  
--main :: IO ()
----main = someFunc "mychart.svg"
--signal :: [Double] -> [(Double,Double)]
--signal xs = [ (x,(sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))) | x <- xs ]
--
--main = toFile def "example1_big.png" $ do
--    layout_title .= "Amplitude Modulation"
--    setColors [opaque blue, opaque red]
--    plot (line "am" [signal [0,(0.5)..400]])
--    plot (points "am points" (signal [0,7..400]))

nt =10
ts =[1..fromIntegral (2*nt)]:: [Double] 
recorded_points :: Curve  
recorded_points = [ (t,(sin (t*3.14159/fromIntegral nt)  )) | t<- ts]
main = 
  let
    rn=0.5
    recorded=[0,1,2,3,4]
    defaultState=0
    phi = (mbphiMaker recorded defaultState)
  in
    print (phi 4 rn)
    
--  let  
--    p= toProb(0.9)
--    nt = 2^5
--    ts :: [Double] 
--    ts =[1..nt]
--  
--    recorded_points ::  [(Double,Double)]
--    recorded_points = [ (t,(sin (t*2*3.14159/40))) | t<- ts]
--    
--    nos = 40
--    fn ="app.svg"
--  in 
--    do 
--      print p
--      -- A random Number from an IO action 
--      -- will be different at each run
--      generator <- getStdGen 
--      let 
--        curveList=[ 
--          ("recorded",recorded_points) 
--          ,("first order mc",(reproduced_points recorded_points nos generator)) 
--          ,("second order mc",(reproduced_points_2 recorded_points nos generator))
--          ]
--      
--      b <- doesFileExist fn
--      if b
--        then putStr ("File " ++ fn ++ " exists. It will be removed")
--      else 
--        putStr "new file"
--
--      toFile def fn $ do
--        layout_title .= "Amplitude Modulation"
--        setColors [opaque blue, opaque red, opaque green]
--        mapM_ (\ (label,points) -> plot(line label [points])) curveList
  
--      let 
--        n=4
--        fn ="errors" ++ (show n) ++".svg"
--        generator = mkStdGen n
--        nos=5
--        recorded_values= [ v | (_,v)<-recorded_points]
--        x_min = minimum recorded_values
--        x_max = maximum recorded_values
--        ts= [ t | (t,v)<-recorded_points]
--
--        (x2ind,ind2x) = ( discretize x_min x_max nos)
--        recorded_state_ids ::  [State]
--        recorded_state_ids = [ (x2ind s) | s <- recorded_values ]
--        tup_0 = (recorded_state_ids !! 0, recorded_state_ids !! 1)
--        nt = length ts
--        --phi = phiTwo_maker recorded_state_ids 
--        --first we create a list of all tuples of successive states in the training
--        --set 
--        tss:: [TwoState] 
--        tss = S.toList (S.fromList (tupleList recorded_state_ids)) 
--        nots = length tss
----        --for every encountered TwoState find the possible successor states and 
----        --the relative abundance (as proxy to probability) with which they occure
--        tps =allTupleTargetProbs(tss) recorded_state_ids
----        --At the moment the 'random' numbers are known the trajectory becomes deterministic
----        
--        phi :: TwoStateTransformer
--        phi 
--          tup -- present state consisting of the (previous_state,current_state)
--          rn --random number 
--          =
--          let 
--            cjps = tupleLookUp tps tup in (targetState cjps rn) 
--        rnums = take (nt-2) ( randomRs (0.0,1.0) generator ::[Double])
--        indList = (ind_traj_2 tup_0 phi rnums)
--      in 
--        do
--          --(print fn)
--          --print (rnums!!13)
--          print tps
--          print (tupleLookUp tps (2,0) )
--          --print (phi (2,0) 0.6270898966134656)
--          --print (take 15 indList)
--          toFile def fn $ do
--            layout_title .= "Amplitude Modulation"
--            setColors [opaque blue, opaque red]
--            plot (line "am" [zip ts (map ind2x indList)])
