cube::(Num a)=> a->a
cube x = x^3

maxi::(Ord a)=> a->a->a
maxi x y    | x>=y      = x
            | otherwise = y

sumAtoB::(Integral a)=>a->a->a 
sumAtoB a b = sum [a..b]
