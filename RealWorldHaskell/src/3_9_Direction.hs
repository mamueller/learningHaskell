--Section 3 exercise 9
--Consider three two-dimensional points a, b, and c. If we look at the angle formed by the line segment from a to b and the line segment from b to c, it either turns left, turns right, or forms a straight line. Define a Direction data type that lets you represent these possibilities.
data CartVec = CartVec {x::Double, y::Double} 
    deriving (Show)

--Points are location vectors from the origin 
type Point = CartVec 

data Direction = MyLeft
    |MyStraight
    |MyRight
    deriving (Show)
p1 :: Point
p1 = CartVec{x=1, y=1}
p2 :: CartVec 
p2 = CartVec {x=2, y=2}
p3 :: CartVec 
p3 = CartVec {x=3, y=4}
p4 :: CartVec 
p4 = CartVec {x=4, y=4}

minus ::CartVec -> CartVec ->CartVec
minus ( CartVec x1 y1) (CartVec x2 y2 ) = CartVec (x1-x2) (y1-y2)

direction :: CartVec -> CartVec -> CartVec ->Direction
direction p q r
    | cp > 0 = MyLeft
    | cp == 0 = MyStraight
    | cp < 0 = MyRight
    where cp = cp_z (q `minus` p) (r `minus` q)  


cp_z:: CartVec -> CartVec -> Double
cp_z (CartVec {x=px,y=py}) (CartVec {x=qx,y=qy}) =  px*qy - py*qx

