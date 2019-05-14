-- Note:
-- note that you have to have the res= whenn
-- writing a program while it is not necessary -- in ghci
-- div is integer division (rest is thrown away)
res=(foldr f 0 l,foldl f 0 l)
    where   l = [6,9,8,3,10] 
            f =(\x a ->(x+a) `div` 2)

res2=foldr (++) [] [ [1,2,3],[4,5,6],[],[7]]

