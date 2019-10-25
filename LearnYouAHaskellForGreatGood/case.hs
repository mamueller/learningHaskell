head' :: [a] -> a
head' [] = error "No head for empty lists."
head' (x:_) = x

headcase :: [a] -> a
headcase xs = case xs of    []      -> error "No head for empty lists."
                            (x:_)   -> x
data SmallNumber = ONE|TWO|THREE|FOUR

toInt :: SmallNumber -> Int
toInt ONE   = 1
toInt TWO   = 2
toInt THREE = 3
toInt FOUR  = 4

toIntcase :: SmallNumber -> Int
toIntcase x = case x of ONE   -> 1
                        TWO   -> 2
                        THREE -> 3
                        FOUR  -> 4


