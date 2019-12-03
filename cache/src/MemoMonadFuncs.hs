
module MemoMonadFuncs
--import Control.Monad.Memo
    ( fibm
    ,evalFibm
    ) where

import Control.Monad.Identity
import Control.Monad.Memo

---- We can rewrite this definition as a monad:
--fibm :: (Eq n,Num n ,Ord n, Monad m) => n -> m n
--fibm 0 = return 0
--fibm 1 = return 1
--fibm n = do
--  f1 <-fibm (n-1)
--  f2 <-fibm (n-2)
--  return (f1+f2)
----and even run it with Identity monad with identical inefficiency:
--evalFibmId :: Integer -> Integer
--evalFibmId = runIdentity . fibm

--But all we need to do to make this function "computable" for
--reasonable argument is to add memoization for both recursive
--branches with memo combinator:

--fibm :: (MonadMemo Integer Integer m) => Integer -> m Integer
fibm 0 = return 0
fibm 1 = return 1
fibm n = do
  f1 <- memo fibm (n-1)
  f2 <- memo fibm (n-2)
  return (f1+f2)

--then, to evaluate it with default Data.Map based memoization cache
--we use the following "eval*" function:

evalFibm :: Integer -> Integer
evalFibm = startEvalMemo . fibm
