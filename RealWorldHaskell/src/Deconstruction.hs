data Shape  = Circle Double 
            | Rectangle Double Double

describe :: Shape ->String
describe (Circle radius)            = "Circle with radus" ++ (show radius)
describe (Rectangle width length)   = "Rectangle with widht=" ++ (show width) ++ " and length=" ++ (show length)                          
