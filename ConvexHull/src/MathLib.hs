module MathLib
    ( 
         add5
        ,direction
        ,threeElTuples
        ,minus
        ,leftmostLowest 
        ,angle
        --Types and constructors
        ,CartVec(..) --(..) to also export the constructors
        ,Point
        ,Direction(..) --(..) to export MyLeft MyRight MyStraight
    ) 
where
data CartVec = CartVec {x::Double, y::Double} 
    deriving (Show,Eq)

--Points are location vectors from the origin 
type Point = CartVec 

data Direction = MyLeft
    |MyStraight
    |MyRight
    deriving (Show,Eq)

--functions
add5 :: Int -> Int
add5 x = x + 5

minus ::CartVec -> CartVec ->CartVec
minus ( CartVec x1 y1) (CartVec x2 y2 ) = CartVec (x1-x2) (y1-y2)

cp_z:: CartVec-> CartVec -> Double
cp_z (CartVec {x=px,y=py}) (CartVec {x=qx,y=qy}) =  px*qy - py*qx

direction :: (Point,Point,Point) ->Direction
direction (p, q, r)
    | cp > 0 = MyLeft
    | cp == 0 = MyStraight
    | cp < 0 = MyRight
    where cp = cp_z (q `minus` p) (r `minus` q)  

threeElTuples:: [a]-> [(a,a,a)]
threeElTuples [p,q,r]=[(p,q,r)]
threeElTuples (p:(q:(r:xs))) = [(p,q,r)]  ++ threeElTuples (q:(r:xs)) 

compareDistanceTo2:: Double ->Double ->Ordering
compareDistanceTo2 x y = compare (dist2 x) (dist2 y) 
    where dist2 x = abs (2-x)

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

norm:: CartVec-> Double
norm p  =  sqrt (dot p p) 

dot:: CartVec-> CartVec -> Double
dot (CartVec {x=px,y=py}) (CartVec {x=qx,y=qy}) =  px*qx + py*qy

angle:: CartVec-> CartVec -> Double
angle p q =  acos ((dot p q) / ((norm p) * (norm q)))
     
