module General
    ( 
      --types
      State
      ,TwoState
      ,ListState
      ,StateTransformer
      ,TwoStateTransformer
      ,ListStateTransformer
      ,Curve
      --functions
      ,discretize
      ,targetProbs
      ,targetFrequencies
      ,cAR1
      ,cARRec
      ,mbTargetState
      ,lSum
      ,findIntervalFromLeft 
      ,curtail 
    ) where
-- In the most simple case a discrete state is represented by an Integer 
type State = Int 
type TwoState = (State,State)
type ListState = [State]

-- a state transformer takes a state and a (supposedly) random number and produces 
-- a new state. It is a completely deterministic function  
type StateTransformer = State -> Double -> State 
type TwoStateTransformer = TwoState -> Double -> State 
type ListStateTransformer = ListState -> Double -> State 
type Curve=[(Double, Double)]

--countAndRemove1 counts the occurences of the state in the list ss and
--and returnes list without any s
cAR1 :: State-> [State] -> (Int,[State])
cAR1 s ss =
  foldl 
    (
      \(n,new_ss) el -> 
        if (el ==s) 
          then (n+1,new_ss) 
        else
          (n,el:new_ss)
    )
    (0,[])
    ss
  

--counts the occurences of the first state in the state list and
--puts it in a tupel (s,n) and appends the tupel to a list
--that is empty at the beginning.
--In the returned state list all occurences of s are removed
--This is then repeated with the second state ...
--until the original states have all been removed from ss
--end the list of tupels is complete
cARRec :: ([(State,Int)], [State]) -> ([(State,Int)], [State])
cARRec (tups, states)
  |length(states) == 0 = (tups, states)
  |length(states) > 0 = 
    let 
      s:rs = states
      (n,new_rs) = cAR1 s rs
    in
      cARRec ((s,n+1):tups, new_rs)

targetFrequencies :: [State] ->[(State,Int)] 
targetFrequencies ss = 
  let (tups,_) = cARRec ([],ss)
  in tups

--computes the probabilities as relative frequency of the targets 
targetProbs:: [(State,Int)] -> [(State,Double)]
targetProbs tups=
  let fsum = ( foldl (\acc (s,f) -> acc+f) 0 tups)
  in  map (\(s,f) -> (s,(fromIntegral f)/(fromIntegral fsum))) tups  

mbTargetState:: (Maybe [(State,Double)]) -> Double -> (Maybe State) 
mbTargetState 
  mbtups -- list of tuples of the form Maybe[(s1, prob1),] 
  rn   -- number between 0 and 1 supposedly generated 'randomly' 
  = 
    mbtups >>= 
      (\tups -> 
          let
            states = map fst tups 
            probs = map snd tups
            i = findIntervalFromLeft (lSum probs) rn
          in 
            Just (states !! i)
      )
      
-- discrete distribution function 
-- summing up probabilities from right to left 
rSum :: [Double] -> [Double] 
rSum l 
  |(length l) ==0 =[0]
  |(length l) ==1 =l
  |(length l) >1 =
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
  |(length fs) ==1 = 0
  |otherwise = (ffl fs rn 0)

ffl :: [Double] -> Double-> Int -> Int
ffl fs rn n = 
  let
    f:fss = fs 
  in
    if rn < f
      then n
    else 
      ffl fss rn (n+1)


discretize :: Double -> Double -> Int-> ((Double  -> Int), (Int -> Double)) 
discretize x_min x_max nos = let 
  batch_width = (x_max - x_min) / (fromIntegral nos)
  x2ind = (\ x  -> floor ((x-x_min) / batch_width))
  ind2x = (\ ind  -> (fromIntegral ind + 0.5) * batch_width + x_min)
  in (x2ind,ind2x) 


curtail :: Double -> [(Int, Double)]  -> Maybe[(Int, Double)] 
curtail threshold tups =
  let
    remaining =filter (\(state,prob) -> prob > threshold ) tups 
  in 
    if remaining == [] then
      Nothing
    else
      let 
        p_sum = foldl (\acc (s,prob) -> acc+prob) 0 remaining
      in 
        Just (map (\(state,prob) ->(state,prob/p_sum)) remaining)
        
