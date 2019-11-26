import Data.Function (fix)

-- normal recursive fact
fact :: Int -> Int 
fact 0 = 1
fact n = n * fact (n-1) 

-- Up to now 'fact' calls itself in the last line on the argumen (n-1)
-- We now create a function g that
-- accepts an argument 'rec' for the function to be called on (n-1)
-- and returns a function 

g :: (Int -> Int) -> Int -> Int
g rec  0 = 1
g rec  n = n* rec (n-1) 

-- Obviously we can reproduce our original 'fact' by calling g on 'fact'
-- So we have g fact =  fact, in other words: The function 'fact' is a
-- fixpoint of the operator g.
-- The following function are all identical fact, fact2 , fact3
fact2 = g fact
fact3 = fix g

-- The surprising ability of the fix point operator fix to find the 
-- function 'fact' as the fixpoint of the operator g is even more 
-- surprising considering its implementation    
myFix :: (a -> a) -> a
myFix f = let {x = f x} in x


fact4 = myFix g

-- different example
my_sqrt :: Double -> Double -> Double -> Double
my_sqrt guess tol val = if abs(guess^2-val) < tol  
  then guess 
  else my_sqrt ((guess + val / guess) / 2.0) tol val

-- reformulation with my_sqrt replaced by a dummy parameter next 
g2 :: (Double -> Double -> Double -> Double) -> Double -> Double -> Double -> Double
g2 next guess tol val = if abs(guess^2-val) < tol  
  then guess 
  else next ((guess + val / guess) / 2.0) tol val

my_sqrt2 = fix g2
my_sqrt3 = myFix g2
