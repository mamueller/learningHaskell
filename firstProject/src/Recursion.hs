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
    
-- with max (which takes only 2 numbers)

maximum5 :: (Ord a) => [a] -> a
maximum5 [] = error "maximum of an empty list"
maximum5 [x] = x
maximum5 (x:xs) = max x ( maximum5 xs )



replicate1 :: (Num i , Ord i ) => i -> a ->[a]
replicate1 n x
    | n < 1 = []
    | n == 1 = [x] -- this middle condition can be removed
    | n > 1 = x:replicate1 (n-1) x

replicate2 :: (Num i , Ord i) => i -> a ->[a]
replicate2 n x
    | n < 1 = []
    | n >= 1 = x:replicate2 (n-1) x

replicate3 :: (Num i , Ord i) => i -> a ->[a]
replicate3 n x
    | n <  1 = []
    | otherwise = x:replicate3 (n-1) x


--take1 :: (Num i, Ord i ) => i->[a]->[a]
--take1 n xs = if  (n <= 0) || (xs == []) then [] else [head xs] ++ take1 (n-1) (tail xs)

take1 :: (Num i, Ord i ) => i->[a]->[a]
take1 n _ 
    | n <= 0    =[]
take1 _ []      =[]
take1 n (x:xs) =x : take1 (n-1) xs

reverse1 :: [a]->[a]
reverse1 [] = []
reverse1 (x:xs) = reverse1 xs ++ [x]

-- an infinete list 
repeat1 :: a -> [a]
repeat1 x = x:repeat1 x 

zip1 :: [a]->[b] -> [(a,b)]
zip1 [] _ = []
zip1 _ [] = []
zip1 (x:xs) (y:ys) = (x,y):zip1 xs ys

elem1 ::(Eq a) => a -> [a] -> Bool
elem1 e [] = False
elem1 e (x:xs)
    | e==x          = True
    | otherwise     = e `elem1` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs , a <= x]
        biggerSorted  = quicksort [a | a <- xs , a > x ]
    in  smallerSorted ++ [x] ++ biggerSorted 
