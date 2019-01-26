module SortLib
    (  
        quickSortBy
    ) where


quickSortBy::(a->a->Ordering) ->[a] ->[a]
quickSortBy cf []  =[]
quickSortBy cf [x] = [x]
quickSortBy cf (p:xs) = (quickSortBy cf (smaller cf p xs ) ) ++ p:(quickSortBy cf (bigger cf p xs) )
    where   smaller cf p xs  =[ x | x <-xs ,(x `cf` p) == LT ] 
            bigger cf p xs =[ x | x <-xs ,(x `cf` p) == GT ||  (x `cf` p) ==EQ]

