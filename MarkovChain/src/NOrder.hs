module NOrder 
  (
      nList
      ,recList
      ,oneList
    ) where
--import Graphics.Rendering.Chart.Easy
--import Graphics.Rendering.Chart.Backend.Diagrams(toFile)
--import Data.Matrix (matrix, getElem) 
--import qualified Data.Set as S -- (fromList,Set,toList)
----import Data.List (unfoldr)
--import System.Random 
--import System.Directory
--import Probability

nList:: [a] -> Int -> [[a]]
nList ys o = 
  let 
    (_,ls) = ( recList (ys,[]) o )
  in 
    reverse ls

recList:: ([a],[[a]])-> Int -> ([a],[[a]])
recList (l,ls) o
  |((length l) <=o) = oneList (l,ls) o
  | otherwise = 
    let
      tup =(oneList (l,ls) o)
    in 
      (recList tup o)
    

oneList:: ([a],[[a]]) -> Int -> ([a],[[a]])
oneList (l,ls) o 
  |((length l) < o) = (l,ls)
  |((length l) >= o) = 
    let 
      x:rest = l
      part = take o l
    in
      (rest,part:ls)

    
