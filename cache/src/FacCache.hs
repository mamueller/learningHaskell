module FacCache
    ( fac, FacCache,caching_fac
    ) where

-- take this faculty computation as an example for somethin expensive that would definitely 
-- benefit from caching
-- abbreviation of the description of the arguments 

type ArgType = Int
type ResType = Int

fac :: ArgType-> ResType
fac 0 = 0
fac 1 = 1
fac n= n* fac (n-1) 

-- now we handcraft a cached version 


type FacCacheKey = ArgType 
type FacCacheRecord= (FacCacheKey,ResType)

-- firts by creating a type for the cache
-- This cache is implemented by an association list of tuples
-- The 'fst' part of the tuple is the argument tuple of a call to fib the 'snd' 
-- part is the result of that call ( in this case a  factorial number )
type FacCache = [FacCacheRecord]

update:: FacCache ->FacCacheRecord -> Int->FacCache 
update fc record max_size = if (length fc) < max_size 
  then fc++[record] 
  else (tail fc) ++[record]

-- The new function takes two  additional arguments:
--  1.) a FacCache representing the known combinations 
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
caching_fac :: Int-> FacCache ->Int -> (Int,FacCache)
caching_fac n fc size= let 
  argTup= n
  new= fac n --this is not computed unless needed (lazyness)
  in case (lookup argTup fc) of
    Nothing  -> (new , (update fc (argTup,new) size))
    Just xs -> (xs,fc) 


