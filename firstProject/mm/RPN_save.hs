import Data.List
import Control.Monad
--solveRPN :: String -> Double
--solveRPN = head . foldl foldingFunction [] . words 

solveRPN :: String -> Maybe Double
solveRPN st  = do
    [result] <- foldM foldingFunction []  (words st)
    return result

foldingFunction :: [Double] -> String -> Maybe [Double] 
foldingFunction (x:y:ys) "*" = return ((x*y):ys) 
foldingFunction (x:y:ys) "+" = return ((x+y):ys)
foldingFunction (x:y:ys) "-" = return ((y-x):ys)
-- foldingFunction (x:y:ys) "/" = (y/x):ys 
-- foldingFunction (x:y:ys) "^" = (y ** x):ys
-- foldingFunction (x:ys) "ln" = log x:ys
-- foldingFunction xs "sum" = [sum xs]
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x,"")] -> Just x
                                _ -> Nothing
-- example readMaybe "1" :: Maybe Int
