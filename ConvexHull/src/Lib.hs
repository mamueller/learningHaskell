module Lib
    ( someFunc
    ,  sayYo
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunction"

sayYo:: String -> String
sayYo input = "yo " ++ input ++"!"
