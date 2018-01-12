--import Data.List (nub,sort)
import qualified Data.Map as Map
import qualified Data.Set as Set
--numUnique :: (Eq a) => [a] -> Int
--numUnique = length . nub

phoneBook =   
        [("betty","555-2938")  
        ,("bonnie","452-2928")  
        ,("patsy","493-2928")  
        ,("lucille","205-2928")  
        ,("wendy","939-8282")  
        ,("penny","853-2492")  
        ]  

findKey :: (Eq k) => k ->[(k,v)]->Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs) = if key ==k 
    then Just v
    else findKey key xs


findKey2 :: (Eq k) => k ->[(k,v)]->Maybe v
findKey2 key = foldl (\acc (k,v) -> if key == k then Just v else acc) Nothing 

findKey3 :: (Eq k) => k ->[(k,v)]->Maybe v
findKey3 key = foldr (\ (k,v) acc -> if key == k then Just v else acc) Nothing 

