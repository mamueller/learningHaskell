import Control.Monad
type KnightPos =(Int,Int)
type ReversedRoute = [KnightPos]

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
    (c',r') <- [ (c+2,r-1)
                ,(c+2,r+1)
                ,(c-2,r-1)
                ,(c-2,r+1)
                ,(c+1,r-2)
                ,(c+1,r+2)
                ,(c-1,r-2)
                ,(c-1,r+2)
               ]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c',r')

in2 :: KnightPos -> [KnightPos]
in2 start = return start >>= moveKnight >>= moveKnight 

canReachIn2 :: KnightPos -> KnightPos -> Bool
canReachIn2 start end = end `elem` in2 start

addRoutesFromSingleRoutes:: ReversedRoute->[ReversedRoute]
addRoutesFromSingleRoutes positions =
    let 
        lastPos=head positions
        possiblePositions=moveKnight(lastPos)
    in map (\pos -> pos:positions) possiblePositions

addRoutesFromListOfRoutes::[ReversedRoute]->[ReversedRoute]
addRoutesFromListOfRoutes listOfRoutes = 
    foldl (\acc route-> acc ++ addRoutesFromSingleRoutes(route)) [] listOfRoutes 

paths2:: KnightPos -> [ReversedRoute]
paths2 pos = addRoutesFromListOfRoutes(addRoutesFromSingleRoutes([pos]))

paths:: [ReversedRoute]-> Int -> [ReversedRoute]
paths routes 1 = addRoutesFromListOfRoutes routes
paths routes n = paths (addRoutesFromListOfRoutes routes) (n-1)

in3 :: KnightPos -> [KnightPos]
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight 

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

canReachIn3Mm :: KnightPos -> KnightPos -> [ReversedRoute]
canReachIn3Mm start end = filter (\route -> (head route) == end) (paths [[start]] 3)

inMany :: Int -> KnightPos ->[KnightPos]
inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)

canReachIn :: Int ->KnightPos -> KnightPos -> Bool
canReachIn x start end = end `elem` inMany x start
