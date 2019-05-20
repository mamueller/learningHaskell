-- Given a set of points represented by their coordinates (x,y) in a 2-D space, 
-- the distance of a point of coordinates (x,y) from the origin is defined as sqrt(x**2+y**2)
-- Define an appropriate type in Haskell and the corresponding overloading operation <= 
-- to handle these points in a priority queue! We always want the point closer to the 
-- origin to be at the front of the queue.
module FivePointTwo
    (   
        PQueue(..)
        ,emptyPQ
        ,pqEmpty
        ,enPQ
        ,dePQ
        ,frontPQ
    )
where
import Point

-- implement the queue        
data PQueue a = PQ {elements::[a],compFunc::(a->a->Bool)}
--newtype PQueue a = PQ [a]
--    deriving (Show,Eq)


emptyPQ :: (a->a->Bool )-> PQueue a
emptyPQ compFunc  = PQ []  compFunc

pqEmpty :: PQueue a -> Bool
pqEmpty (PQ [] _) = True
pqEmpty _       = False

enPQ :: (Ord a) => a -> PQueue a -> PQueue a
--(enPQ x (PQ q f)) = (PQ (insert x q) f )
enPQ x (PQ {elements=q,compFunc=f}) = PQ {elements=insert x q,compFunc=f} 
    where   insert x []         = [x]
            insert x r@(e:r')   | x <= e    = x:r
                                | otherwise = e: (insert x r')

dePQ :: (Ord a)=> PQueue a -> PQueue a 
dePQ (PQ [] _ ) = error "dePQ: empty priority queue"
dePQ (PQ (x:r) f) = PQ r f

frontPQ :: (Ord a) => PQueue a -> a
frontPQ (PQ [] _ ) = error "frontPQ: empty priority list"
frontPQ (PQ (x:r)_ ) = x

