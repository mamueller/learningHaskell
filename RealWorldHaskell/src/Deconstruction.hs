data Shape  = Circle Double 
            | Rectangle Double Double

describe :: Shape ->String
describe (Circle radius)            = "Circle with radus" ++ (show radius)
describe (Rectangle width length)   = "Rectangle with widht=" ++ (show width) ++ " and length=" ++ (show length)                          
-- alternative Solution with record syntax which  adds functions radius widht and length automatically 
-- unfortunately lenght collides with 
data Shape2  = Circle2 {radius::Double} 
            | Rectangle2 {width:: Double, length::Double}
              deriving (Show)
              
c2::Shape2
c2 = Circle2 3
r2::Shape2
r2=Rectangle2 2 3
describe2 :: Shape2 ->String
describe2 (Circle2 x)  = show (radius c) 
    where c=Circle2 x
describe2 (Rectangle2 x y)  =show ( width r) ++ show (Main.length r)  
    where r=Rectangle2 x y

describe3 :: Shape2 ->String
describe3 (Circle2 x)  = 
    let c = Circle2 x
    in show (radius c) 
describe3 (Rectangle2 x y)  =
    let r=Rectangle2 x y 
    in show ( width r) ++ show (Main.length r)  
   
