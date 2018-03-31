import System.Random
import Data.List

main = do
    gen <- getStdGen
    putStrLn $ take 20 (randomRs ('a','z') gen )
    gen' <- newStdGen
    putStrLn $ take 20 (randomRs ('a','z') gen')
--    -- putStr $ randomRs ('a','z') gen
--    putStr . show $ threeCoins gen
--    putStr . show $ threeCoins (mkStdGen 21)
--    putStr . show $ threeCoins (mkStdGen 22)
--    print ( take 5 $ randoms (mkStdGen 11) :: [Int])
---- random (mkStdGen 100) :: (Int, StdGen)
--threeCoins :: StdGen -> (Bool,Bool,Bool)
--threeCoins gen =
--    let (firstCoin, newGen) = random gen
--        (secondCoin,newGen') = random newGen
--        (thirdCoin,newGen'') = random newGen'
--    in  (firstCoin,secondCoin,thirdCoin)
