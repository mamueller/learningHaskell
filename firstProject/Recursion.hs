maximum1 :: (Ord a) => [a] -> a
maximum1 [] = error "maximum of an empty list"
maximum1 [x] = x
maximum1 (x:xs) = if x > maxTail  then x else maxTail
    where maxTail= maximum1 xs


maximum2 :: (Ord a) => [a] -> a
maximum2 [] = error "maximum of an empty list"
maximum2 [x] = x
maximum2 (x:xs) = 
    let maxTail = maximum2 xs 
    in ( if x > maxTail  then x else maxTail) 

maximum3 :: (Ord a) => [a] -> a
maximum3 [] = error "maximum of an empty list"
maximum3 [x] = x
maximum3 (x:xs) = 
    let maxTail = maximum3 xs 
        cond = x > maxTail
    in case cond of True -> x
                    False -> maxTail

-- with guards
maximum4 :: (Ord a) => [a] -> a
maximum4 [] = error "maximum of an empty list"
maximum4 [x] = x
maximum4 (x:xs)  
   | x > maxTail =x
   | otherwise = maxTail
     where maxTail= maximum1 xs
    

