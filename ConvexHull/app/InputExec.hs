module Main where
import Lib
import MathLib
main :: IO()
main = do
    print "Hello, what is your name?"
    name <- getLine
    print "What is your favorite number"
    numberStr <-getLine
    print "Let's add 5!"
    print $ add5 (read numberStr :: Int)

