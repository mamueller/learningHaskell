module FibCache
    ( fib, FibCache,caching_fib
    ) where

-- take this fibonacci list computation as an example for somethin expensive that would definitely 
-- benefit from caching
-- abbreviation of the description of the arguments 

fib :: Int -> Int -> Int -> [Int]
fib x0 y0 1 = [x0]
fib x0 y0 2 = [x0,y0]
fib x0 y0 n = let l= fib x0 y0 (n-1)
                  rl=reverse l
                  y= head rl
                  x= head (tail rl)
                  in l ++[x+y]

----------------------------------------------------------------------------
-- now we handcraft a cached version 

type FibCacheKey = (Int,Int,Int)
type FibCacheRecord= (FibCacheKey,[Int])

-- firts by creating a type for the cache
-- This cache is implemented by an association list of tuples
-- The 'fst' part of the tuple is the argument tuple of a call to fib the 'snd' 
-- part is the result of that call ( in this case a list of fibonacci number )
type FibCache = [FibCacheRecord]

update:: FibCache ->FibCacheRecord -> Int->FibCache 
update fc record max_size = if (length fc) < max_size 
  then fc++[record] 
  else (tail fc) ++[record]

-- The new function takes two  additional arguments:
--  1.) a FibCache representing the known combinations 
--      that can be used to look up the result before embarking in the long computation
--  2.) an Integer that limits tha number of cached results
-- 
-- It also returns something different:
-- Instead of a just the result of the fib computation 
-- it returns a tuple of 
-- 1.) the result of the fib computation 
-- 2.) a (possibly) updated cache 
-- Where in case a computation had to be done after all the new record (argtupel,result) 
-- is added to the cache and in case that the size of the cache would be exeedec the oldest
-- element is removed.
caching_fib :: Int-> Int -> Int -> FibCache ->Int -> ([Int],FibCache)
caching_fib x0 y0 n fc size= let 
                              argTup= (x0,y0,n)
                              new= fib x0 y0 n
                              in case (lookup argTup fc) of
                                Nothing  -> (new , (update fc (argTup,new) size))
                                Just xs -> (xs,fc) 

----------------------------------------------------------------------------
-- as second handcrafted version that works only on a reversed list
-- the arguments are
--    a list of fibonachi numbers of length l (possibly 0 at the beginning) 
--    an integer n, the desired length of the list
-- the result is not the n-th fibonachi number but the list of the first n fibonachi numbers 

listFib :: [Int] -> Int -> [Int]
listFib [] 1 =  [0]
listFib [] 2 =  [1,0]
listFib xs n  |n < (m + 1) = take n xs
              |n ==(m + 1) = (head xs + head (tail xs)): xs
              |n > (m + 1) = (head ys + head (tail ys)): ys
              where  m =length xs                               
                     ys = listFib xs (n-1) 
