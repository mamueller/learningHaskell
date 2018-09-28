smaller :: Ord a => a -> [a] -> [a]
smaller p xs = [ x | x <- xs,x<p]

bigger :: Ord a => a -> [a] -> [a]
bigger p xs = [ x | x <- xs,x>p]

quicksort :: Ord a => [a]->[a]
quicksort []=[]
quicksort [x]=[x]
quicksort (p:xs) = quicksort (smaller p xs) ++ p:(quicksort (bigger p xs))

