module Lib
    ( make_svg
      , discretize
      , reproduced_points
      , ind_traj
      , phi_maker
      , lSum
      ,findIntervalFromLeft 
    ) where
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams(toFile)
import Data.Matrix (matrix, getElem) 
import Data.Set(fromList)
--import Data.List (unfoldr)
import System.Random 
import System.Directory


-- In the most simple case a discrete state is represented by an Integer 
type State = Int 
type StateTransformer = State -> State 

make_svg :: [(Double, Double)] ->  [(Double, Double)] -> FilePath -> IO ()
make_svg recorded_points reproduced_points filePath = toFile def filePath $ do
    layout_title .= "Amplitude Modulation"
    setColors [opaque blue, opaque red]
    plot (line "recorded_points" [recorded_points])
    plot (line "reproduced_points" [reproduced_points])


discretize :: Double -> Double -> Int-> ((Double  -> Int), (Int -> Double)) 
discretize x_min x_max nos = let 
  batch_width = (x_max - x_min) / (fromIntegral nos)
  x2ind = (\ x  -> floor ((x-x_min) / batch_width))
  ind2x = (\ ind  -> (fromIntegral ind + 0.5) * batch_width + x_min)
  in (x2ind,ind2x) 


phi_maker :: [State]-> StateTransformer 
phi_maker recorded_state_ids = 
  let
    ss = fromList recorded_state_ids
    nos = length ss
    jumpProb :: (Int, Int) -> Double
    jumpProb (i,j) 
      | i == (j+1) = 1 
      | otherwise =0
    
    mat = matrix nos nos jumpProb
    
    phi :: StateTransformer
    phi ind 
      | ind < nos = ind+1
      | otherwise = nos
  in 
    phi


ind_traj :: State -> StateTransformer ->Int ->[State] 
ind_traj  s0 phi nos = 
  let 
    ind_traj_reverse :: State -> StateTransformer ->Int ->[State] 
    ind_traj_reverse  s0 phi nos
      |nos == 0 = []
      |nos == 1 = [s0]
      |otherwise = let 
        ss_old = ind_traj_reverse s0 phi (nos - 1) 
        in (phi (head ss_old)):ss_old
    
  in 
    reverse (ind_traj_reverse s0 phi nos)

reproduced_points :: [(Double,Double)] -> Int -> StdGen -> [(Double,Double)]
reproduced_points recorded_points nos generator = 
  let 
    recorded_values= [ v | (_,v)<-recorded_points]
    x_min = minimum recorded_values
    x_max = maximum recorded_values
    x_0 = head recorded_values
    ts= [ t | (t,v)<-recorded_points]

    (x2ind,ind2x) = ( discretize x_min x_max nos)
    recorded_state_ids ::  [Int]
    recorded_state_ids = [ (x2ind s) | s <- recorded_values ]
    ind_0 = head recorded_state_ids
    phi = phi_maker recorded_state_ids
    nt = length ts
    indList = (ind_traj ind_0 phi nt)
    rnums = take nt ( randomRs (0.0,1.0) generator ::[Double])
  in 
    zip ts (map ind2x indList)
  
-- discrete distribution function 
-- summing up probabilities from right to left 
rSum :: [Double] -> [Double] 
rSum l =
  if (length l) ==1 
    then l
  else
    let 
      x:xs = l
      nxs = rSum xs
    in
      (x+ (head nxs)): nxs

-- discrete distribution function 
lSum :: [Double] -> [Double] 
lSum ps = reverse (rSum (reverse ps))

findIntervalFromLeft :: [Double] -> Double-> Int
findIntervalFromLeft fs rn 
  |(length fs) ==1 = 1
  |otherwise = (ffl fs rn 1)

ffl :: [Double] -> Double-> Int -> Int
ffl fs rn n = 
  let
    f:fss = fs 
  in
    if rn < f
      then n
    else 
      ffl fss rn (n+1)

--ul :: Int -> StdGen-> ([Double],StdGen) 
--ul n gen
--  |n==0 = ([], gen)
--  |n>0 =
--    let 
--      (ress,gen1) = ul (n-1) gen
--      (res,gen2) = uniformR (0.0 ,1.0) gen1
--    in (res:ress,gen2)
