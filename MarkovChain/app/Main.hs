module Main where

import Lib
import System.Directory

import Control.Monad  
import Data.Char  
import System.Random 
import System.Directory
import Probability
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams(toFile)
  
--main :: IO ()
----main = someFunc "mychart.svg"
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
          ,("first order mc",(reproduced_points recorded_points nos generator)) 
          ,("second order mc",(reproduced_points_2 recorded_points nos generator))
          ]
      
      b <- doesFileExist fn
      if b
        then putStr ("File " ++ fn ++ " exists. It will be removed")
      else 
        putStr "new file"

      toFile def fn $ do
        layout_title .= "Amplitude Modulation"
        setColors [opaque blue, opaque red, opaque green]
        mapM_ (\ (label,points) -> plot(line label [points])) curveList
  
