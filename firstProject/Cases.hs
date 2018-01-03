head1 :: [a] -> a
head1 [] = error "No head' for an empty list"
head1 (x:_) =x 

head2 :: [a] -> a
head2 xs = case xs of   [] -> error "No head for empty lists"
                        (x:_) -> x

--describeList :: [a] -> String
--describeList xs = "The list is " ++ case xs of  []->"empty"
--                                                [x]->"a singleton"    
--                                                xs ->"a longer list"    
describeList :: [a] -> String
describeList xs = "The list is " ++ what xs 
    where   what [] = "empty"
            what [x] = "a singleton"    
            what xs = "a longer list"    
