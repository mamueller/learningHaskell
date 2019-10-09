data Tree =Leaf|Node Int Tree Tree deriving Show

maxVal :: Tree -> Int
maxVal (Node x Leaf Leaf)   =  x 
maxVal (Node x Leaf st)     =  max x (maxVal st)
maxVal (Node x st Leaf)     =  max x (maxVal st)
maxVal (Node x lst rst)     =   let leftMax =maxVal lst
                                    rightMax=maxVal rst
                                in  max x (max leftMax rightMax)

minVal :: Tree -> Int
minVal (Node x Leaf Leaf)   =  x 
minVal (Node x Leaf st)     =  min x (minVal st)
minVal (Node x st Leaf)     =  min x (minVal st)
minVal (Node x lst rst)     =   let leftMin=minVal lst
                                    rightMin=minVal rst
                                in  min x (min leftMin rightMin)

-- I had to avoid that minVal or maxVal get called on a tree that consists of a single Leaf
-- Therefore the pattern matching is a bit "exhaustive" in this function.
-- If I do not do this then the ghc correctly complains about the patterns in minVal and maxVal being non exhaustive
-- which is of cause intended but a bit dangerous.
isSortedTree :: Tree -> Bool
isSortedTree Leaf =True
isSortedTree (Node x Leaf Leaf) = True
isSortedTree (Node x Leaf rst) = (isSortedTree rst) && ((minVal rst) >=x)
isSortedTree (Node x lst Leaf) = (isSortedTree lst) && ((maxVal lst) <x)
isSortedTree (Node x lst rst) =  (isSortedTree lst) 
                                    && ((maxVal lst) <x)
                                    && (isSortedTree rst) 
                                    && ((minVal rst) >=x)

-- I can write the function more elegantly by encapsulating 
-- and maxVal t , x into a maxValSmaller:: Tree->Int->Bool   function (and the same for minVal t >= x)
-- which would get rid of the problem that minVal and maxVal applied to a Leaf do not make sense.
--
maxValSmaller :: Tree->Int->Bool
maxValSmaller Leaf x =True
maxValSmaller (Node xt lst rst) x  =  (xt < x) && (maxValSmaller lst x) && (maxValSmaller rst x)

minValLargerEqual :: Tree->Int->Bool
minValLargerEqual Leaf x =True
minValLargerEqual (Node xt lst rst) x  =  (xt >= x) && (minValLargerEqual lst x) && (minValLargerEqual rst x)

isSortedTree3 :: Tree -> Bool
isSortedTree3 Leaf =True
isSortedTree3 (Node x lst rst) =  (isSortedTree lst) 
                                    && (maxValSmaller lst x) 
                                    && (isSortedTree rst) 
                                    && (minValLargerEqual rst x)



