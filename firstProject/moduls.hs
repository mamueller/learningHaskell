import Data.List (nub,sort)
numUnique :: (Eq a) => [a] -> Int
numUnique = length . nub
