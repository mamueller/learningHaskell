palindrome :: [a]->[a]
palindrome [] =[]
palindrome [x] = [x]
palindrome (x:xs) = x:(palindrome xs)++[x]
