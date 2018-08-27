module Lib
    ( someFunc
    , listlength
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

--listlength:: Num p => [a] ->p
listlength:: Num p => [a] ->p
listlength [] = 0
listlength ( x:xs) =1 + (listlength xs)
