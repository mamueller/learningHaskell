-- Section 3 Exercise 7
-- Define a function that joins a list of lists together using a separator value.
intersperse :: a -> [[a]] ->[a]
intersperse _ [] = []
intersperse _ [l] = l
intersperse sep (l:ls) = l++[sep]++(intersperse sep ls)  
