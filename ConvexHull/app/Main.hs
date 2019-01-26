module Main where

import Lib
import MathLib
import SortLib
import qualified Data.List as L
--Section 3 exercise 9-11
--Consider three two-dimensional points a, b, and c. If we look at the angle formed by the line segment from a to b and the line segment from b to c, it either turns left, turns right, or forms a straight line. Define a Direction data type that lets you represent these possibilities.


p1 :: Point
p1 = CartVec{x=1, y=1}
p2 :: CartVec 
p2 = CartVec {x=2, y=2}
p3 :: CartVec 
p3 = CartVec {x=3, y=4}
p4 :: CartVec 
p4 = CartVec {x=4, y=4}
p5 :: Point
p5 = CartVec{x=4, y=1}





l :: [Point]
l=[p1,p2,p3,p4,p5]

--directions ::[Point]->[Direction]
--directions l = map direction ( threeElTuples l ) 


lL::Point
lL=leftmostLowest l

--order all points except lL by the angle their connection with lL

-- define a function that computes the angle between the vecor lL->p and the x-axis
lL_p_angle_with_x_axis::Point->Double
lL_p_angle_with_x_axis p = angle (p `minus` lL) (CartVec 1 0)


--define the comparison func:
compareByAngleWithX:: Point->Point->Ordering
compareByAngleWithX p1 p2 = compare ( f p1 ) (f p2)
    where f=lL_p_angle_with_x_axis

-- create the list of points without lL
all_except_lL::[Point]
all_except_lL=L.delete lL l
ordered::[Point]
ordered=quickSortBy compareByAngleWithX all_except_lL

stack:: [Point]
stack = take 2 l

grahamStep:: ([Point],[Point])->([Point],[Point])
--this function first checks if the new point p forms a left or right turn
--with the 2 topmost stack points
grahamStep ((nc:rcs),(l:(sl:ps)))
    |((dir == MyLeft)|| (dir== MyStraight)) = (rcs, nc:l:sl:ps) 
    |(dir == MyRight) = (rcs, nc:l:sl:ps) 
        where dir = direction(sl,l,nc)

main :: IO ()
main = do
    someFunc
    print( sayYo "Haskellers")
    --print (add5 4)
    putStrLn ("l="++(show l))
    putStrLn ("lL="++(show lL))
    print ("ordered="++ (show ordered))
    print (grahamStep (ordered, stack))
    
