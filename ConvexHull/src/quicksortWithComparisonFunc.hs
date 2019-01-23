
quicksortWithComparisonFunc::(a->a->Ordering) ->[a] ->[a]
quicksortWithComparisonFunc cf []  =[]
quicksortWithComparisonFunc cf [x] = [x]
quicksortWithComparisonFunc cf (p:xs) = (quicksortWithComparisonFunc cf (smaller cf p xs ) ) ++ p:(quicksortWithComparisonFunc cf (bigger cf p xs) )
    where   smaller cf p xs  =[ x | x <-xs ,(x `cf` p) == LT ] 
            bigger cf p xs =[ x | x <-xs ,(x `cf` p) == GT ||  (x `cf` p) ==EQ]

compareDistanceTo2:: Double ->Double ->Ordering
compareDistanceTo2 x y = compare (dist2 x) (dist2 y) 
    where dist2 x = abs (2-x)

l=[1,2,3,4,5]
dl=quicksortWithComparisonFunc compareDistanceTo2 l
