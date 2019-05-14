-- Given a set of points represented by their coordinates (x,y) in a 2-D space, 
-- the distance of a point of coordinates (x,y) from the origin is defined as sqrt(x**2+y**2)
-- Define an appropriate type in Haskell and the corresponding overloading operation <= 
-- to handle these points in a priority queue! We always want the point closer to the 
-- origin to be at the front of the queue.
module FivePointOne
    (   
         Point(..) --(..) to also export the constructors
        ,dist
        ,emptyPQ
        ,pqEmpty
        ,enPQ
        ,dePQ
        ,frontPQ
    )
where
data Point = Point {x::Double,y::Double}
    --deriving (Show,Eq,Ord)
    deriving (Show,Eq)
    --deriving (Show)

dist :: Point -> Double
dist p = sqrt ( (x p)**2 + (y p)**2)

instance Ord Point
    where 
        x <= y = (dist x <= dist y)
        x >= y = (dist x >= dist y)
        
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

