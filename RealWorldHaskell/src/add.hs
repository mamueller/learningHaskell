--pattern matchihg example
myNot::Bool->Bool
myNot True = False
myNot False= True

sumList::Num a => [a]->a
sumList (x:xs)  = x + sumList xs
sumList []      = 0
