module Main where

import Lib
import System.Directory

import Control.Monad  
import Data.Char  
import System.Random 
  
--main :: IO ()
----main = someFunc "mychart.svg"
main = 
  let  
    nt = 400 
    ts :: [Double] 
    ts =[0,(0.5)..nt]
  
    recorded_points ::  [(Double,Double)]
    recorded_points = [ (t,(sin (t*3.14159/45)  )) | t<- ts]
    
    nos = 10
    fn ="mychart.svg"
  in 
    do 
      -- A random Number from an IO action 
      -- will be different at each run
      generator <- getStdGen 
      
      b <- doesFileExist fn
      if b
        then putStr ("File " ++ fn ++ " exists. It will be removed")
      else 
        putStr "new file"

      putStr (show b)
      make_svg  recorded_points  (reproduced_points recorded_points nos generator) fn 

  
