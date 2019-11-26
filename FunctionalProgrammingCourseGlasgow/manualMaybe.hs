myHead :: [a] -> Maybe a
myHead [] = Nothing
myHead (x:xs) = Just x

myTail :: [a] -> Maybe [a]
myTail [] = Nothing
myTail (x:xs) = Just xs

myThird :: [a] -> Maybe a
myThird xs =
  case myTail xs of 
    Nothing -> Nothing
    Just a -> case myTail a of 
            Nothing -> Nothing
            Just b -> myHead b

myThirdWithMonad :: [a] -> Maybe a
myThirdWithMonad xs =
  myTail xs >>= \xs -> myTail xs >>= (\xs -> myHead xs)
