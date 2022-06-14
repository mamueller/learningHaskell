module Lib
    ( 
      bt
    , reject
    , accept
    ) where

import qualified Data.Array as A 
import qualified Data.List as L 
import qualified Data.Set as S 


possible :: (Integer,Integer) -> Bool
possible (i,j) = True

type Soduku = A.Array (Int, Int) (Maybe Int) 
empty_field :: Soduku 
empty_field  =  
    let
        min_i=0
        max_i=8
        l=[min_i..max_i]
    in
        A.array ((min_i,min_i),(max_i,max_i)) [ ((i,j),Nothing ) |i<-l ,j<-l ]


--practice backtracking by finding all unique list of {1,2,3} of lenght 3 
reject :: [Integer]-> Bool
reject c 
    | (length(L.nub c) == length(c)) = False 
    | otherwise = True 

accept :: [Integer]-> Bool
accept c = (length c == 3)


bt :: [[Integer]] -> [Integer] -> [[Integer]]
bt sols pc
    | (reject pc) = sols
    | (accept pc) = sols ++ [pc]
    | otherwise = let
       new_pcs = [ pc++[i] | i<-[1..3]]
       new_sols = [(bt [] npc) | npc <- new_pcs]
       in foldl (\acc el -> acc++el) sols new_sols

