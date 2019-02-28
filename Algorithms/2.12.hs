import Data.Array

--(lower,higher)[(index,value)|index<-[lower..higher]]
--arr1= array (1,2) [(1,"a"),(2,"b")] 
--
--arr2=listArray(1,2)["a","b"]
--truth=arr1==arr2

a=listArray (1,4)[11,20,36,47]
b=listArray (1,15)[1..15]
c=listArray (2,11)[f x |x<-[2..11] ]
    where f x   | (x `mod` 2==0) = x
                | otherwise = -x
