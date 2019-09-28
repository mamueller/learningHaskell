module Main where

import System.Random
import Lib

main :: IO ()
main = do
        ind <-randomRIO (1,length dict ) :: IO Int 
        -- putStrLn (dict !! ind)
        starman (dict !! ind) 5 

