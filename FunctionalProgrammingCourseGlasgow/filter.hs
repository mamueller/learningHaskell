myfilter :: (a->Bool )->[a] ->[a]
myfilter f [] = []
myfilter f (x:xs) 
    | (f x) = x: (myfilter f xs)
    | otherwise = myfilter f xs

