palindromeCheck :: Eq a => [a] -> Bool
palindromeCheck xs = reverse xs == xs
