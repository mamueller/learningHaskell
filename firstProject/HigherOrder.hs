applyTwice :: (a->a) -> a ->a
applyTwice f x = f (f x )


make_fmultOne::(Num a) => a -> a -> a
make_fmultOne y z = ( * y) z

multTwo::(Num a) => a ->a ->a
--multTwo y z = (make_fmultOne y) z
multTwo y = (make_fmultOne y) 

make_fmultTwo::(Num a) => a -> a -> a ->a 
make_fmultTwo x y z = x * multTwo y z

multThree ::(Num a) => a -> a ->a ->a
--multThree x y z = (make_fmultTwo x) y z
multThree x = (make_fmultTwo x) 

zipWith1 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith1 _ []_=[]
zipWith1 _ _[]=[]
zipWith1 f (x:xs) (y:ys) = f x y : zipWith1 f xs ys

flip1 :: (a->b->c)->(b->a->c)
flip1 f x y = f y x
--flip1 f = g
--    where g x y = f y x
--

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = smallerSorted ++ [x] ++ biggerSorted
    where   smallerSorted= quicksort (filter (<=x) xs)
            biggerSorted = quicksort (filter (>x) xs)

largestDivisible ::(Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x`mod` 3829 == 0
    
collatzChain :: (Integral a) => a -> [a]
collatzChain 1 = [1]
collatzChain n
    | even n = n:collatzChain (n `div` 2)
    | odd n =  n:collatzChain (n*3 + 1)


numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map collatzChain [1..100]))

sum1 :: (Num a) => [a] -> a
sum1 xs = foldl (\s x -> s + x ) 0 xs

sum2 :: (Num a) => [a] -> a
sum2 xs = foldl (+) 0 xs

elem1 :: (Eq a) => a -> [a] -> Bool
elem1 e l = foldl (\acc x -> if x==e then True else acc) False l

map1 :: (a->b)->[a] -> [b]
map1 f xs = foldl (\acc x -> acc ++ [f x]) [] xs
