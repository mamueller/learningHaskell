module Point
    (
         Point(..) --(..) to also export the constructors
        ,dist
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
        
