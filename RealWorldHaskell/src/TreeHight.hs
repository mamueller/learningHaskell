-- Section 3 Exercise 8
-- Using the binary tree type that we defined earlier in this chapter, write a function that will determine 
-- the height of the tree. The height is the largest number of hops from the root to an Empty. 
-- For example, the tree Empty has height zero; 
-- Node "x" Empty Empty has height one; 
-- Node "x" Empty (Node "y" Empty Empty) has height two; and so on.
data Tree a = Node a (Tree a) (Tree a)
        |Empty
        deriving (Show)

et :: Tree String
et = Node "a" Empty (Node "b" Empty (Node "c" Empty Empty)) 

treeHight :: Tree a -> Integer
treeHight Empty = 0
treeHight (Node _ t1 t2) =1 + max (treeHight t1) (treeHight t2) 
