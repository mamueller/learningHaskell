module Main where

--import Lib
import FibCache
import FacCache
import Control.Monad.Memo


c0 :: FibCache
--c0= [((1,2,1),[1]) ,((1,2,2),[1,2]) ,((1,2,3),[1,2,3]) ,((1,2,4),[1,2,3,5]) ]
c0= []

d0:: FacCache
d0 =[]

main :: IO ()
main = 
  let fibArgs = (1,2,5)
      (x0,y0,n)=fibArgs
      fib_result=fib x0 y0 n
      fib_resAndCache=caching_fib x0 y0 n c0 2
      cached_fib_result=fst fib_resAndCache
      fib_cache=snd fib_resAndCache
      m=10
      fac_resAndCache=caching_fac m d0 2
      cached_fac_result=fst fac_resAndCache
      fac_cache=snd fac_resAndCache

    in do 
        putStrLn  ("lookup fib in c0 = " ++ show (Prelude.lookup fibArgs c0))
        putStrLn  ("computed fib = " ++ show (fib_result))
        putStrLn  ("cached fib = " ++ show (cached_fib_result))
        putStrLn  ("fib cache = " ++ show (fib_cache))
        
        putStrLn  ("lookup fac in d0" ++ show (Prelude.lookup n d0))
        putStrLn  ("computed fac = " ++ show (fac m))
        putStrLn  ("cached fac = " ++ show (cached_fac_result))
        putStrLn  ("fac cache = " ++ show (fac_cache))
