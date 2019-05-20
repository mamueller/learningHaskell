-- Given a set of points represented by their coordinates (x,y) in a 2-D space, 
-- the distance of a point of coordinates (x,y) from the origin is defined as sqrt(x**2+y**2)
-- Define an appropriate type in Haskell and the corresponding overloading operation <= 
-- to handle these points in a priority queue! We always want the point closer to the 
-- origin to be at the front of the queue.
module FivePointThree
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
    )
where
import Point
-- implement the queue        
newtype Set a = Set {elementlist::[a]}
    deriving (Show)

instance (Eq a)=>Eq (Set a)
    where 
        s1 == s2 =and [ (included s1 s2) , (included s2 s1)]

emptySet ::(Eq a)=> Set a
emptySet =Set []

setEmpty ::Set a -> Bool
setEmpty (Set []) = True
setEmpty _       = False

addSet ::(Eq a)=>  a -> Set a -> Set a
addSet x (Set q) = Set (x:q)

delSet ::(Eq a)=> a -> Set a -> Set a 
delSet _ (Set [] ) = error "delSet: cannot delelte from empty set"
delSet x (Set xs) = Set (filter (/= x) xs) 

inSet :: (Eq a)=> Set a -> a -> Bool
inSet s p = or [ x==p |x <- (elementlist s )]   
               
included :: (Eq a)=> Set a -> Set a ->Bool
included s1 s2 = and [ inSet s2 x| x <- elementlist s1]  

inter :: (Eq a)=> Set a -> Set a -> Set a 
inter s1 s2 = Set [ x| x <- elementlist s1,(inSet s2 x)]  

union :: (Eq a)=> Set a -> Set a -> Set a 
union s1 s2 = Set ( (elementlist s1) ++ (elementlist s2))

