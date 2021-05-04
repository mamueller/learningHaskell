module Main where

import General
import NOrder
import System.Directory

import Control.Monad  
import Data.Char  
import qualified Data.Set as S -- (fromList,Set,toList)
import System.Random 
import System.Directory
import Probability
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams(toFile)
  

nt =10
ts =[1..fromIntegral (2*nt)]:: [Double] 
recorded_points :: Curve  
recorded_points = [ (t,(sin (t*3.14159/fromIntegral nt)  )) | t<- ts]

main = 
  let  
    p= toProb(0.9)
    nt = 2^5
    ts :: [Double] 
    ts =[1..nt]
  
    recorded_points ::  [(Double,Double)]
    recorded_points = [ (t,(sin (t*2*3.14159/40))) | t<- ts]
    
    nos = 40
    fn ="app.svg"
  in 
    do 
      print p
      -- A random Number from an IO action 
      -- will be different at each run
      generator <- getStdGen 
      let 
        curveList=[ 
          ("recorded",recorded_points) 
          ,("first order mc",(listReproducedPoints recorded_points 1 nos generator)) 
          ,("second order mc",(listReproducedPoints recorded_points 2 nos generator)) 
          ]
      
      b <- doesFileExist fn
      if b
        then putStr ("File " ++ fn ++ " exists. It will be removed")
      else 
        putStr "new file"

      toFile def fn $ do
        layout_title .= "First vs second order Markov Chain"
        setColors [opaque blue, opaque red, opaque green]
        mapM_ (\ (label,points) -> plot(line label [points])) curveList
  
