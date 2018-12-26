--Section 3 exercise 9
--Consider three two-dimensional points a, b, and c. If we look at the angle formed by the line segment from a to b and the line segment from b to c, it either turns left, turns right, or forms a straight line. Define a Direction data type that lets you represent these possibilities.
data Point = Point {x::Double, y::Double} 
    deriving (Show)

data Direction = MyLeft
    |MyStraight
    |MyRight
    deriving (Show)
p1 :: Point 
p1 = Point {x=1, y=1}
p2 :: Point 
p2 = Point {x=2, y=2}
p3 :: Point 
p3 = Point {x=3, y=4}
p4 :: Point 
p4 = Point {x=4, y=4}

direction :: Point -> Point -> Point ->Direction
direction p q r = MyLeft 
