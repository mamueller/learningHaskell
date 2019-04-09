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
f:: [Int]->[Int]->[Int]
f =    \ a b -> (head a) : [ (head b) ] 
f_a =  \ a b -> (f $!a)    b 
f_b =  \ a b -> (f   a) $! b 
f_ab = \ a b -> (f $!a) $! b

g:: [Int]->[Int]
g = \x -> 1:(2:x)

glog :: [Int]-> Writer[String] [Int]
glog = \ x -> do  
                tell ["applied g"]
                return (g x)
                
--f_log :: Writer [String] [Int]->Writer [String] [Int]->Writer [String] [Int]
--f_log  (writer (a,msgsa)) (writer( b,msgsb))  =  do 
--                tell ["applied f"]
--                return (f a b)
fm =liftM2 f
res_strict_a  = fm  (glog $! [3,4]) (glog  [5,6 ])
res_strict_b  = fm (glog  [1]) (glog $! [2])
res_strict_ab = fm (glog $! [1]) (glog $! [2])
--
