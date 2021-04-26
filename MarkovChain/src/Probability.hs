module Probability ( 
toProb
) where
--since we do not export the value constructor P
--the only way to create a Prob value outside the module is 
--to use the exported toProb function
--which checks that the argument Double is really a Probabiliyt


newtype Prob = P Double
  deriving Show

toProb :: Double -> Maybe Prob
toProb p = if ((p>=0) && (1>=p)) then Just (P p) else Nothing
