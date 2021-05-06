module NOrder 
  (
      nList
      ,recList
      ,oneList
      ,recSuccessors
      ,listSuccessors
      ,allListTargetProbs
      ,simplifiedAllListTargetProbs
      ,mbListCondJumpProbs 
      ,mbSimplifiedListCondJumpProbs 
      ,listIndTrajReverse 
      ,listIndTraj
      ,listPhiMaker
      ,simplifiedListPhiMaker
      ,listReproducedPoints
    ) where
import qualified Data.Set as S -- (fromList,Set,toList)
import System.Random 
import General
--import Graphics.Rendering.Chart.Easy
--import Graphics.Rendering.Chart.Backend.Diagrams(toFile)
--import Data.Matrix (matrix, getElem) 
----import Data.List (unfoldr)
--import System.Directory
--import Probability

nList:: [a] -> Int -> [[a]]
nList ys o = 
  let 
    (_,ls) = ( recList (ys,[]) o )
  in 
    reverse ls

recList:: ([a],[[a]])-> Int -> ([a],[[a]])
recList (l,ls) o
  |((length l) <=o) = oneList (l,ls) o
  | otherwise = 
    let
      tup =(oneList (l,ls) o)
    in 
      (recList tup o)
    

oneList:: ([a],[[a]]) -> Int -> ([a],[[a]])
oneList (l,ls) o 
  |((length l) < o) = (l,ls)
  |((length l) >= o) = 
    let 
      x:rest = l
      part = take o l
    in
      (rest,part:ls)


--recursion over the remaining shortening src and accumulation of hits
recSuccessors:: [State]-> [State] -> [State] -> ([State],[State]) 
recSuccessors pattern src hits=
    let 
      np=length pattern
      ns=length src
    in 
      if ns <= np then --src is shorter than pattern or there is no successor 
        (src,hits)
      else
        let
          (comp,rest) = splitAt np src
          x:newsrc =src
        in 
          if comp == pattern then
            recSuccessors pattern newsrc ((head rest):hits)
          else
            recSuccessors pattern newsrc hits
                
        
listSuccessors:: [State]-> [State] -> [State]
listSuccessors pattern src = let
  (_,hits) = (recSuccessors pattern src [])
  in 
    hits

--the data structure of all conditional jump probabilities for all states
--At the moment it is a list but a binary search tree would be 
--more efficient for lookups
allListTargetProbs::[ListState] -> [State] -> [([State], [(State,Double)])]
allListTargetProbs
  ss --states 
  rs --recorded states
  = foldl
      (\ acc s -> 
        let mbtl = mbListCondJumpProbs rs s
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


--compute the jump probs for a given State from a given trajectory
mbListCondJumpProbs ::[State] -> ListState ->  (Maybe[(State,Double)])
mbListCondJumpProbs
  rs --recorded states
  s --present state
  = let 
      succs = listSuccessors s rs
      tfs = targetFrequencies succs
      l= if succs ==[] then 
          Nothing
        else
          Just (targetProbs tfs)
    in 
       l

--the data structure of all conditional jump probabilities for all states
--At the moment it is a associative list but a binary search tree would be 
--more efficient for lookups
simplifiedAllListTargetProbs::[ListState] -> [State] -> Double -> [([State], [(State,Double)])]
simplifiedAllListTargetProbs
  ss --states 
  rs --recorded states
  threshold --minimal probability to be considered
  = foldl
      (\ acc s -> 
        let mbtl = mbSimplifiedListCondJumpProbs rs s threshold
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

--compute the jump probs for a given State from a given trajectory
--remove the target states
--
mbSimplifiedListCondJumpProbs ::[State] -> ListState -> Double ->  (Maybe[(State,Double)])
mbSimplifiedListCondJumpProbs
  rs --recorded states
  s --present state
  threshold --minimal probability to be considered
  = 
    let
      mb = mbListCondJumpProbs rs s
    in 
      mb >>= (curtail threshold)


--Starting with a listState (lenght n for a N-th order chain) we add as many
--new states as we have random numbers
--for performance reasons we add to the result on the left
--the initial state is also given in reverse order
listIndTrajReverse :: ListState -> ListStateTransformer ->[Double]->[State] 
listIndTrajReverse  
  listState --startState (reversed order)
  phi 
  rns --random Numbers
  | rns ==[]  = listState
  | otherwise =
    let 
      rn:rrns =rns
      ss_prev = listIndTrajReverse listState phi rrns 
      newListState = take (length listState) ss_prev
    in (phi newListState rn):ss_prev
        

--Starting with a listState (lenght n for a N-th order chain) we add as many
--new states as we have random numbers
--while we internally build the list by adding from the left
--this function reverses the result
listIndTraj :: ListState -> ListStateTransformer -> [Double] -> [State] 
listIndTraj  
  listState_0 --initial listState (first n states for a Nth order MC) 
  phi -- the (listState) tranformer function 
  rns -- numbers (supposedly randomly generated elsewhere)
      -- that will determine the length of the trajectory
      -- which will have the same length as rns
  = let 
      revListState_0=(reverse listState_0)
      rev_phi listState = phi (reverse listState)
      rev_traj = listIndTrajReverse revListState_0 rev_phi rns
    in
      reverse (rev_traj)

--create a state transformer by cataloging all the possible successive values
--of a given ListState  (n previous states for a nth. order MC
--where the present state here is  represented by n successive values)
listPhiMaker :: [State] -> Int -> State ->  ListStateTransformer 
listPhiMaker 
  recorded_state_ids
  order
  defaultState
  = 
  let
    --first we create a list of all n-element lists of successive states in the training
    --set 
    tss:: [ListState] 
    tss = S.toList (S.fromList (nList recorded_state_ids order)) 
    nots = length tss
--    --for every encountered ListState find the possible successor states and 
--    --the relative abundance (as proxy to probability) with which they occure
    tps =allListTargetProbs(tss) recorded_state_ids 
--    --At the moment the 'random' numbers are known the trajectory becomes deterministic
--    
    phi :: ListStateTransformer
    phi 
      listState -- present state consisting of the n previous_states
      rn --random number 
      =
        let 
          mbcjps = lookup listState tps 
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
--of a given ListState  (n previous states for a nth. order MC
--where the present state here is  represented by n successive values)
--but discard those jumb probabilities that are smaller than threshhold
simplifiedListPhiMaker :: [State] -> Int -> Double -> State ->  ListStateTransformer 
simplifiedListPhiMaker 
  recorded_state_ids
  order
  threshold
  defaultState
  = 
  let
    --first we create a list of all n-element lists of successive states in the training
    --set 
    tss:: [ListState] 
    tss = S.toList (S.fromList (nList recorded_state_ids order)) 
    nots = length tss
--    --for every encountered ListState find the possible successor states and 
--    --the relative abundance (as proxy to probability) with which they occure
    tps =simplifiedAllListTargetProbs(tss) recorded_state_ids threshold
--    --At the moment the 'random' numbers are known the trajectory becomes deterministic
--    
    phi :: ListStateTransformer
    phi 
      listState -- present state consisting of the n previous_states
      rn --random number 
      =
        let 
          mbcjps = lookup listState tps 
          mbts = mbTargetState mbcjps rn
        in 
          if mbts == Nothing then
            defaultState
          else
            let Just ts = mbts
            in  ts
  in 
    phi


--use a markov chain of order order
listReproducedPoints :: [(Double,Double)] -> Int -> Int -> StdGen -> [(Double,Double)]
listReproducedPoints 
  recorded_points --trajectory to learn from
  order           --order of the MC
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
    defaultState =  head recorded_state_ids
    listState_0 = take order recorded_state_ids
    nt = length ts
    phi = listPhiMaker 
      recorded_state_ids
      order
      defaultState
    rnums = take nt ( randomRs (0.0,1.0) generator ::[Double])
    indList = (listIndTraj listState_0 phi rnums)
  in 
    zip ts (map ind2x indList)
