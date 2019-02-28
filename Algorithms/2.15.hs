import Data.Array
--define example
ex_arr=array((1,1),(3,3))[
    ((1,1),2)
    ,((1,2),3)
    ,((1,3),4)
    ,((2,1),5)
    ,((2,2),6)
    ,((2,3),7)
    ,((3,1),8)
    ,((3,2),9)
    ,((3,3),10)
    ]
--define my own !
mm_get::(Ix a)=>Array a b->a->b 
arr `mm_get` ind = (arr ! ind)
res = ex_arr `mm_get` (2,2)
--define my own bounds
mm_bounds::(Ix a)=>Array a b -> (a,a)
mm_bounds arr= bounds arr
--define my own indices
mm_indeces:: (Ix a)=>Array a b ->[a]
mm_indeces arr = indices arr
--define my own elems 
mm_elems:: (Ix a)=>Array a b ->[b]
mm_elems arr = elems arr
