
module MemoMonadFuncs
--import Control.Monad.Memo
    ( fibm
    ,evalFibmId
    ) where

import Control.Monad.Identity

fibm :: (Eq n,Num n ,Ord n, Monad m) => n -> m n
fibm 0 = return 0
fibm 1 = return 1
fibm n = do
  n1 <-fibm (n-1)
  n2 <-fibm (n-2)
  return (n1+n2)

evalFibmId :: Integer -> Integer
evalFibmId = runIdentity . fibm
