data  Shape a = Rect {x::a,y::a,func :: a -> Bool}
            |Circ a
area::(Floating a)=> Shape a->a
area (Circ val) = val**2*pi
            --deriving (Show)
