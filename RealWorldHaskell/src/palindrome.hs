palindrome :: [a]->[a]
palindrome [] =[]
palindrome (x:xs) = x:(palindrome xs)++[x]
