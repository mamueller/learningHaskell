module Lib
    ( 
      --types
      State
      ,TwoState
      ,StateTransformer
      ,TwoStateTransformer
      ,Curve
      --functions
      ,discretize
      ,reproduced_points
      ,reproduced_points_2
      ,ind_traj
      ,ind_traj_2
      ,ind_traj_reverse_2 
      ,phiMaker
      ,phiTwo_maker
      ,lSum
      ,findIntervalFromLeft 
      ,findSuccessors
      ,findTupleSuccessors
      ,targetLists 
      ,targetFrequencies
      ,targetProbs
      ,targetState
      ,mbTargetState
      ,mbCondJumpProbs 
      ,tupleCondJumpProbs 
      ,allTargetProbs
      ,allTupleTargetProbs
      ,cAR1
      ,cARRec
      ,tupleLookUp
      ,tupleList
      --,nList
      --,recList
      --,oneList
    ) where
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams(toFile)
import Data.Matrix (matrix, getElem) 
import qualified Data.Set as S -- (fromList,Set,toList)
--import Data.List (unfoldr)
import System.Random 
import System.Directory
import Probability


-- In the most simple case a discrete state is represented by an Integer 
type State = Int 
type TwoState = (State,State)

-- a state transformer takes a state and a (supposedly) random number and produces 
-- a new state. It is a completely deterministic function  
type StateTransformer = State -> Double -> State 
type TwoStateTransformer = TwoState -> Double -> State 
type Curve=[(Double, Double)]


discretize :: Double -> Double -> Int-> ((Double  -> Int), (Int -> Double)) 
discretize x_min x_max nos = let 
  batch_width = (x_max - x_min) / (fromIntegral nos)
  x2ind = (\ x  -> floor ((x-x_min) / batch_width))
  ind2x = (\ ind  -> (fromIntegral ind + 0.5) * batch_width + x_min)
  in (x2ind,ind2x) 


--create a state transformer by cataloging all the possible successive values
--of a given value (The state is here just represented as the index of a discretized
--value
phiMaker :: [State] -> State ->  StateTransformer 
phiMaker 
  recorded_state_ids
  defaultState
  = 
  let
    ss:: S.Set Int 
    ss = (S.fromList recorded_state_ids) 

    --for every encountered combination of (state,successor) count the number of occurenses
    tps =allTargetProbs(S.toList ss) recorded_state_ids
    --At the moment the 'random' numbers are known the trajectory becomes deterministic
    
    phi :: StateTransformer
    phi 
      s -- present state
      rn --random number
      -- =let cjps = lookUp tps s in (targetState cjps rn) 
      =
        let 
          mbcjps = lookup s tps 
          mbts = mbTargetState mbcjps rn
        in 
          if mbts == Nothing then
            defaultState
          else
            let Just ts = mbts
            in  ts
  in 
    phi

--create a state transformer by cataloging all the possible successive values
--of a given tuple of values 
--(The present state here is  represented by two successive values)
phiTwo_maker :: [State]->  TwoStateTransformer 
phiTwo_maker 
  recorded_state_ids
  = 
  let
    --first we create a list of all tuples of successive states in the training
    --set 
    tss:: [TwoState] 
    tss = S.toList (S.fromList (tupleList recorded_state_ids)) 
    nots = length tss
--    --for every encountered TwoState find the possible successor states and 
--    --the relative abundance (as proxy to probability) with which they occure
    tps =allTupleTargetProbs(tss) recorded_state_ids
--    --At the moment the 'random' numbers are known the trajectory becomes deterministic
--    
    phi :: TwoStateTransformer
    phi 
      tup -- present state consisting of the (previous_state,current_state)
      rn --random number 
      =
      let 
        cjps = tupleLookUp tps tup in (targetState cjps rn) 
  in 
    phi
    

--lookUp :: [(State,[(State,Double)])] -> State ->[(State,Double)]
--lookUp 
--  atps -- list of all tuples of (presentState,[(s1,Prob(s1),...,(sn,Prob(sn))])
--       -- where s1..sn are the possible successor states of s and Prob(sn) is
--       -- the probability of this state.
--  s    -- present state
--  = 
--  let 
--    pred ::(State,[(State,Double)])->Bool
--    pred (st,tups) = st ==s 
--    
--    (_,tprobs) = head (filter pred atps)
--  in tprobs

tupleLookUp :: [(TwoState,[(State,Double)])] -> TwoState ->[(State,Double)]
tupleLookUp 
  atps -- list of all tuples of (presentState,[(s1,Prob(s1),...,(sn,Prob(sn))])
       -- where s1..sn are the possible successor states of s and Prob(sn) is
       -- the probability of this state.
  stup  -- tuple(previous state, current state)
  = 
    (let 
      pred ::(TwoState,[(State,Double)])->Bool
      pred (t,tups) = t ==stup
      hits = filter pred atps

      res = if length hits == 0 
        then []
      else
        let 
          (_,tprobs) = head (hits)
        in 
          tprobs
    in
      res
    )
      
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
      


targetState:: [(State,Double)] -> Double -> State 
targetState 
  tups -- list of tuples of the form (state, prob) 
  rn   -- number between 0 and 1 supposedly generated 'randomly' 
  = 
    if length tups ==0 
      -- if we have never encountered this tuple before 
      -- and consequnetly do not know where do go from here
      -- we return a default state, which works a like a restart value 
      then 0 
    else
      let
        states = map fst tups 
        probs = map snd tups
        i = findIntervalFromLeft (lSum probs) rn
      in states !! i

ind_traj :: State -> StateTransformer -> [Double] -> [State] 
ind_traj  
  s0 --start state
  phi -- the state tranformer function
  rns -- numbers (supposedly randomly generated elsewhere)
      -- that will determine the length of the trajectory
      -- which will have the same length as rns
  = 
  let 
    ind_traj_reverse :: State -> StateTransformer ->[Double]->[State] 
    ind_traj_reverse  s0 phi rns
      |length(rns) == 0 = []
      |length(rns) == 1 = [s0]
      |otherwise = let 
        rn:rrns =rns --
        ss_old = ind_traj_reverse s0 phi rrns 
        in (phi (head ss_old) rn):ss_old
    
  in 
    reverse (ind_traj_reverse s0 phi rns)

--for performance reasons we add to the result on the left
ind_traj_reverse_2 :: TwoState -> TwoStateTransformer ->[Double]->[State] 
ind_traj_reverse_2  tup_0 phi rns
--  |length(rns) == 0 = []
--  |length(rns) == 1 = let (s_0,_)=tup_0 in [s_0]
  |length(rns) == 2 = let (s_0,s_1)=tup_0 in [s_0,s_1]
  |length(rns) >=2 = let 
    rn:rrns =rns --
    ss_old = ind_traj_reverse_2 tup_0 phi rrns 
    tup = (head ss_old,head (tail ss_old))
    in (phi tup rn):ss_old

ind_traj_2 :: TwoState -> TwoStateTransformer -> [Double] -> [State] 
ind_traj_2  
  tup_0 --start Twostate (first two states) 
  phi -- the state tranformer function 
  rns -- numbers (supposedly randomly generated elsewhere)
      -- that will determine the length of the trajectory
      -- which will have the same length as rns
  = let 
      (s0,s1)= tup_0
      rev_tup_0=(s1,s0)
      rev_phi (c,p) = phi (p,c)
      rev_traj = ind_traj_reverse_2 rev_tup_0 rev_phi rns
    in
      reverse (rev_traj)

reproduced_points :: [(Double,Double)] -> Int -> StdGen -> [(Double,Double)]
reproduced_points recorded_points nos generator = 
  let 
    recorded_values= [ v | (_,v)<-recorded_points]
    x_min = minimum recorded_values
    x_max = maximum recorded_values
    ts= [ t | (t,v)<-recorded_points]

    (x2ind,ind2x) = ( discretize x_min x_max nos)
    recorded_state_ids ::  [Int]
    recorded_state_ids = [ (x2ind s) | s <- recorded_values ]
    ind_0 = head recorded_state_ids
    nt = length ts
    defaultState = 0 
    phi = phiMaker recorded_state_ids defaultState 
    rnums = take nt ( randomRs (0.0,1.0) generator ::[Double])
    indList = (ind_traj ind_0 phi rnums)
  in 
    zip ts (map ind2x indList)
  

--use a second order markov chain here
reproduced_points_2 :: [(Double,Double)] -> Int -> StdGen -> [(Double,Double)]
reproduced_points_2 
  recorded_points --trajectory to learn from
  nos             --number of states for the discretesation
  generator       --random number generator
  = 
  let 
    recorded_values= [ v | (_,v)<-recorded_points]
    x_min = minimum recorded_values
    x_max = maximum recorded_values
    ts= [ t | (t,v)<-recorded_points]

    (x2ind,ind2x) = ( discretize x_min x_max nos)
    recorded_state_ids ::  [State]
    recorded_state_ids = [ (x2ind s) | s <- recorded_values ]
    tup_0 = (recorded_state_ids !! 0, recorded_state_ids !! 1)
    nt = length ts
    phi = phiTwo_maker recorded_state_ids 
    rnums = take nt ( randomRs (0.0,1.0) generator ::[Double])
    indList = (ind_traj_2 tup_0 phi rnums)
  in 
    zip ts (map ind2x indList)

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

findSuccessors:: State -> [Int] -> [Int]
findSuccessors x0 xs 
  |(length xs) < 2 = []
  |(length xs) >=2 =
    let 
      f:ys = xs
      s:yss = ys
      res = findSuccessors x0 ys
    in  
      if f==x0 then s:res else res

findTupleSuccessors:: TwoState -> [Int] -> [Int]
findTupleSuccessors tup0 xs 
  |(length xs) < 3 = []
  |(length xs) >=3 =
    let 
      f:ys = xs
      s:t:yss = ys
      res = findTupleSuccessors tup0 ys
    in  
      if (f,s)==tup0 then t:res else res


targetLists :: S.Set State -> [State] -> [(State,[State])]
targetLists ss  sl = [(s,(findSuccessors s sl)) |s <- (S.toList ss)]

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



--compute the jump probs for a given State from a given trajectory
mbCondJumpProbs ::[State] -> State ->  (State,Maybe[(State,Double)])
mbCondJumpProbs
  rs --recorded states
  s --present state
  = let 
      succs = findSuccessors s rs
      tfs = targetFrequencies succs
      l= if succs ==[] then 
          Nothing
        else
          Just (targetProbs tfs)
    in 
       ( s,l)

--compute the jump probs for a given State from a given trajectory
mbCondJumpProbs2 ::[State] -> State ->  (Maybe[(State,Double)])
mbCondJumpProbs2
  rs --recorded states
  s --present state
  = let 
      succs = findSuccessors s rs
      tfs = targetFrequencies succs
      l= if succs ==[] then 
          Nothing
        else
          Just (targetProbs tfs)
    in 
       l

--compute the jump probs for a given TwoState from a given trajectory
tupleCondJumpProbs ::[State] -> TwoState -> (TwoState,[(State,Double)])
tupleCondJumpProbs
  rs --recorded states
  tup --present state tup=(prev,curr)
  = let 
      succs = findTupleSuccessors tup rs
      tfs = targetFrequencies succs
    in 
      (tup,targetProbs tfs)



----the data structure of all conditional jump probabilities for all states
----At the moment it is a list but a binary search tree would be 
----more efficient for lookups
allTargetProbs::[State] -> [State] -> [(State, [(State,Double)])]
allTargetProbs
  ss --states
  rs --recorded states
  = foldl
      (\ acc s -> 
        let mbtl = mbCondJumpProbs2 rs s
        in
          if mbtl==Nothing 
            then acc
          else 
            let 
              Just tl = mbtl
              in (s,tl):acc
      )
      []
      ss

--the data structure of all conditional jump probabilities for all states
--At the moment it is a list but a binary search tree would be 
--more efficient for lookups
allTupleTargetProbs::[TwoState] -> [State] -> [(TwoState, [(State,Double)])]
allTupleTargetProbs
  tups --list of tuples 
  rs --recorded states
  = map (tupleCondJumpProbs rs) tups


--not save for [] as argument
tupleList:: [a] -> [(a,a)]
tupleList ys = let
  x:xs=ys
  in (zip ys xs)

--nList:: [a] -> Int -> [[a]]
--nList ys o = 
--  let 
--    (_,ls) = ( recList (ys,[]) o )
--  in 
--    reverse ls
--
--recList:: ([a],[[a]])-> Int -> ([a],[[a]])
--recList (l,ls) o
--  |((length l) <=o) = oneList (l,ls) o
--  | otherwise = 
--    let
--      tup =(oneList (l,ls) o)
--    in 
--      (recList tup o)
--    
--
--oneList:: ([a],[[a]]) -> Int -> ([a],[[a]])
--oneList (l,ls) o 
--  |((length l) < o) = (l,ls)
--  |((length l) >= o) = 
--    let 
--      x:rest = l
--      part = take o l
--    in
--      (rest,part:ls)
--
--    
