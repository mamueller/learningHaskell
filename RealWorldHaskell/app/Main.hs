module Main where
--import Lib
--import BookStore
import SimpleJSON
import PutJSON

main :: IO ()
--main = do
--     someFunc
--     putStrLn (show myInfo)
main = putJValue (JObject [("foo",JNumber 1),("bar",JBool False)])
