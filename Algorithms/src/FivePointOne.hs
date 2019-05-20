-- Given a set of points represented by their coordinates (x,y) in a 2-D space, 
-- the distance of a point of coordinates (x,y) from the origin is defined as sqrt(x**2+y**2)
-- Define an appropriate type in Haskell and the corresponding overloading operation <= 
-- to handle these points in a priority queue! We always want the point closer to the 
-- origin to be at the front of the queue.
module FivePointOne
    (   
        emptyPQ
        ,pqEmpty
        ,enPQ
        ,dePQ
        ,frontPQ
    )
where
import Point        
-- implement the queue        
newtype PQueue a = PQ[a]
    deriving (Show,Eq)

emptyPQ :: PQueue a
emptyPQ=PQ []

pqEmpty :: PQueue a -> Bool
pqEmpty (PQ []) = True
pqEmpty _       = False

enPQ :: (Ord a) => a -> PQueue a -> PQueue a
enPQ x (PQ q) = PQ (insert x q)
    where   insert x []         = [x]
            insert x r@(e:r')   | x <= e    = x:r
                                | otherwise = e: (insert x r')

dePQ :: (Ord a)=> PQueue a -> PQueue a 
dePQ (PQ [] ) = error "dePQ: empty priority queue"
dePQ (PQ (x:r)) = PQ r

frontPQ :: (Ord a) => PQueue a -> a
frontPQ (PQ [] ) = error "frontPQ: empty priority list"
frontPQ (PQ (x:r)) = x

