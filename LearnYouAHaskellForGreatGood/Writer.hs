import Control.Monad.Writer  
gcd' :: Int -> Int -> Writer [String] Int  
gcd' a b  
    | b == 0 = do  
        tell ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        gcd' b (a `mod` b)  ;


gcd2Mon :: Int -> Int -> Writer [String] Int  
gcd2Mon a b  
    | b == 0 = writer (a,[ "Finished with" ++ show a])
    | otherwise = 
        let 
            wrappedRes =gcd2Mon b (a `mod` b)
            -- runWriter unwraps the result to a tuple
            t= runWriter wrappedRes
            number = fst (t)
            oldLog = snd (t)
            textLines=  [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)] ++ oldLog 
            --writer wraps the tuple into the Monad  
        in writer (number ,textLines)

twoTimesMon :: Int -> Writer [String] Int 
twoTimesMon  x = writer (2*x, ["Multiplied by 2"])

--res = snd (runWriter (gcd' 8 3))
resMon = gcd2Mon 8 3
tup= runWriter resMon
res = fst tup
logList = snd tup
main = mapM_ putStrLn logList 

-- The aim is to see how enforcing strictness for expressions in a function does affect the order of execution
-- We use the writer monad to protocoll what happened.
--
-- Consider the function call f a b! How would you force the strict evaluation of a only, b only, and both a and b?
f:: Int->Int->Int
f = \ a b -> a + b 
g = \ x -> 2*x
h = \ x -> 3*x

res_strict_a = f (g (h 2)) (h (g 2))
