-- a)
neg::(Real a)=> [a]->Int
neg xs = length [x |x<-xs ,x<0]

-- b)
rep:: Int->[Int]
rep n 
    | n ==0 =[]
    | n >0  =(rep (n-1))++[n |x<-[1..n]]
    | otherwise = (error "This function does not make sense for negative Numbers.")
