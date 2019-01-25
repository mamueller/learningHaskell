module SortLib
    (  quickSortBy
    ) where


quickSortBy::(a->a->Ordering) ->[a] ->[a]
quickSortBy cf []  =[]
quickSortBy cf [x] = [x]
quickSortBy cf (p:xs) = (quickSortBy cf (smaller cf p xs ) ) ++ p:(quickSortBy cf (bigger cf p xs) )
    where   smaller cf p xs  =[ x | x <-xs ,(x `cf` p) == LT ] 
            bigger cf p xs =[ x | x <-xs ,(x `cf` p) == GT ||  (x `cf` p) ==EQ]

compareDistanceTo2:: Double ->Double ->Ordering
compareDistanceTo2 x y = compare (dist2 x) (dist2 y) 
    where dist2 x = abs (2-x)
