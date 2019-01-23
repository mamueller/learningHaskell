import qualified Data.List as L
--Section 3 exercise 9-11
--Consider three two-dimensional points a, b, and c. If we look at the angle formed by the line segment from a to b and the line segment from b to c, it either turns left, turns right, or forms a straight line. Define a Direction data type that lets you represent these possibilities.
data CartVec = CartVec {x::Double, y::Double} 
    deriving (Show,Eq)
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
p5 :: Point
p5 = CartVec{x=4, y=1}

minus ::CartVec -> CartVec ->CartVec
minus ( CartVec x1 y1) (CartVec x2 y2 ) = CartVec (x1-x2) (y1-y2)

direction :: (Point,Point,Point) ->Direction
direction (p, q, r)
    | cp > 0 = MyLeft
    | cp == 0 = MyStraight
    | cp < 0 = MyRight
    where cp = cp_z (q `minus` p) (r `minus` q)  


cp_z:: CartVec-> CartVec -> Double
cp_z (CartVec {x=px,y=py}) (CartVec {x=qx,y=qy}) =  px*qy - py*qx

norm:: CartVec-> Double
norm p  =  sqrt (dot p p) 

dot:: CartVec-> CartVec -> Double
dot (CartVec {x=px,y=py}) (CartVec {x=qx,y=qy}) =  px*qx + py*qy

angle:: CartVec-> CartVec -> Double
angle p q =  acos ((dot p q) / ((norm p) * (norm q)))

threeElTuples:: [a]-> [(a,a,a)]
threeElTuples [p,q,r]=[(p,q,r)]
threeElTuples (p:(q:(r:xs))) = [(p,q,r)]  ++ threeElTuples (q:(r:xs)) 
     


l :: [Point]
l=[p1,p2,p3,p4,p5]

directions ::[Point]->[Direction]
directions l = map direction ( threeElTuples l ) 

--from 2 point find point with lowest y coordinate and if y1==y2 with lowest x
leftmostLower:: Point->Point->Point
leftmostLower po pn 
        | yn > yo    = po  
        | yn == yo  = (if xn>xo then po else pn)   
        | otherwise  = pn
        where   yn = y pn
                yo = y po
                xn = x pn
                xo = x po
leftmostLowest::[Point]->Point
leftmostLowest xs = foldl leftmostLower (head xs) xs

lL::Point
lL=leftmostLowest l

--order all points except lL by the angle their connection with lL

-- define a function that computes the angle between the vecor lL->p and the x-axis
lL_p_angle_with_x_axis::Point->Double
lL_p_angle_with_x_axis p = angle (p `minus` lL) (CartVec 1 0)

--smallerAngleSubList :: Point -> [Point] -> [Point]
--smallerAngleSubList pivotPoint points = [ point | point <- points,(lL_p_angle_with_x_axis point) < (lL_p_angle_with_x_axis pivotPoint)]
--
--biggerAngleSubList :: Point -> [Point] -> [Point]
--biggerAngleSubList pivotPoint points = [ point | point <- points,(lL_p_angle_with_x_axis point) >=  (lL_p_angle_with_x_axis pivotPoint)]
--
--quicksortByAngle::[Point]->[Point]
--quicksortByAngle []=[]
--quicksortByAngle [p]=[p]
--quicksortByAngle (p:xs) = (quicksortByAngle (smallerAngleSubList p xs)) ++ [p] ++ (quicksortByAngle (biggerAngleSubList p xs))

-- implement or import a sort function that can work with an arbitrary comparison func
quicksortWithComparisonFunc::(a->a->Ordering) ->[a] ->[a]
quicksortWithComparisonFunc cf []  =[]
quicksortWithComparisonFunc cf [x] = [x]
quicksortWithComparisonFunc cf (p:xs) = (quicksortWithComparisonFunc cf (smaller cf p xs ) ) ++ p:(quicksortWithComparisonFunc cf (bigger cf p xs) )
    where   smaller cf p xs  =[ x | x <-xs ,(x `cf` p) == LT ] 
            bigger cf p xs =[ x | x <-xs ,(x `cf` p) == GT ||  (x `cf` p) ==EQ]

--define the comparison func
compareByAngleWithX:: Point->Point->Ordering
compareByAngleWithX p1 p2 = compare ( f p1 ) (f p2)
    where f=lL_p_angle_with_x_axis

-- create the list of points without lL
all_except_lL::[Point]
all_except_lL=L.delete lL l
ordered::[Point]
ordered=quicksortWithComparisonFunc compareByAngleWithX all_except_lL
