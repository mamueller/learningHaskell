myHead :: [a] -> Maybe a
myHead [] = Nothing
myHead (x:xs) = Just x

myTail :: [a] -> Maybe [a]
myTail [] = Nothing
myTail (x:xs) = Just xs

foo :: [a] -> Maybe a
foo xs =
  case myTail xs of 
    Nothing -> Nothing
    Just a -> case myTail a of 
            Nothing -> Nothing
            Just b -> myHead b
