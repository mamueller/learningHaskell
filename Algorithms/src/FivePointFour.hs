--considering the set implementation by means of ordered lists implement--
module FivePointFour
    (   
        Set(..)
        ,emptySet
        ,setEmpty
        ,addSet
        ,delSet
        ,inSet
        ,included
        ,inter
        ,union
        ,unique
        ,setfromList 
    )
where
import Point
import Data.List (elem)
--helper to build sets from lists with duplicates
--but better build the set with addSet from the elements by folding
--this also ensures the right order of the internal ordered list
unique ::(Eq a)=> [a] ->[a]
unique []  = [] 
unique [x]  = [x] 
unique (x:xs)  = x:(filter (/=x) (unique xs))

-- implement the queue        
newtype Set a = Set {elementlist::[a]}
    deriving (Show)

instance (Ord a)=>Eq (Set a)
    where 
        s1 == s2 =and [ (included s1 s2) , (included s2 s1)]

setfromList :: ( Ord a)=> [a]->(Set a)
setfromList xs = foldl f emptySet xs
    where f  = \ acc x ->(addSet x acc)

emptySet ::(Ord a)=> Set a
emptySet =Set []

setEmpty ::Set a -> Bool
setEmpty (Set []) = True
setEmpty _       = False

addSet ::(Ord a)=>  a -> Set a -> Set a
addSet x (Set q)    |inSet (Set q) x = Set q
                    | otherwise= Set (insert x q)
                        where 
                            insert x [] =[x]
                            insert x r@(e:r')   | x<e = x:r
                                                | otherwise = e:(insert x r')

delSet ::(Ord a)=> a -> Set a -> Set a 
delSet _ (Set [] ) = error "delSet: cannot delelte from empty set"
delSet x (Set xs) = Set (filter (/= x) xs) 

inSet :: (Ord a)=> Set a -> a -> Bool
inSet s p = or [ x==p |x <- (elementlist s )]   
               
included :: (Ord a)=> Set a -> Set a ->Bool
included s1 s2 = and [ inSet s2 x| x <- elementlist s1]  

inter :: (Ord a)=> Set a -> Set a -> Set a 
inter s1 s2 = Set [ x| x <- elementlist s1,(inSet s2 x)]  

union :: (Ord a)=> Set a -> Set a -> Set a 
union s1 s2 = Set ( (elementlist s1) ++ (elementlist s2))

