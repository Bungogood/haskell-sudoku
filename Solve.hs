import Data.Maybe
import Data.List
import Data.Ord
import CSP

-- Forward checking
-- https://en.wikipedia.org/wiki/Look-ahead_(backtracking)

forwardcheck :: CSP -> Assignment -> Var -> (CSP, Bool)
forwardcheck csp a x = recforwardcheck csp a x ys
  where
    ys = allNeighboursOf x csp

recforwardcheck :: CSP -> Assignment -> Var -> [Var] -> (CSP, Bool)
recforwardcheck csp a x [] = (csp, True)
recforwardcheck csp a x (y:ys) =
  if null domain then (csp, False)
  else recforwardcheck (setDomain (y, domain) csp) a x ys
  where
    cs = commonConstraints csp x y
    domain = filter (\i -> checkConstraints cs $ assign (y, i) a) $ getDomain y csp

-- Minimum Remaining Values (MRV)

getMRVVariable :: Assignment -> CSP -> Var
getMRVVariable a = fst . minimumBy (comparing (length . snd)) . domains
  where 
    domains :: CSP -> Domains
    domains = filter (not . isAssigned a . fst) . cspDomains

-- Least Constraining Value (LCV)
-- https://en.wikipedia.org/wiki/Min-conflicts_algorithm

{-
function LCVSort(csp, a, x) returns sorted domain of x
  ys ← x neighbours
  for each v in Dx do
    assign x v
    for each y in ys do
      count Dy which satisfy the constraint between x and y
  return sorted domain

function choices(csp, a, x, v) returns size of Dys if x assigned v
  ys ← x neighbours
  assign x v
  sum for each y in ys do
    count Dy which satisfy the constraint between x and y
  return total
-}

lcvSort :: CSP -> Assignment -> Var -> [Int]
lcvSort csp assignment x = (reverse . sortBy (comparing choices) . flip getDomain csp) x
  where
    ys = filter (not . isAssigned assignment) $ allNeighboursOf x csp -- only check unassigned
    choices :: Int -> Int
    choices v = sum $ map checker ys
      where
        a = assign (x,v) assignment
        checker :: Var -> Int
        checker y = length domain
          where
            cs = commonConstraints csp x y
            domain = filter (\i -> checkConstraints cs $ assign (y, i) a) $ getDomain y csp

-- Most Constraining Value (MCV)

mcvSort :: CSP -> Assignment -> Var -> [Int]
mcvSort csp assignment x = (sortBy (comparing choices) . flip getDomain csp) x
  where
    ys = filter (not . isAssigned assignment) $ allNeighboursOf x csp -- only check unassigned
    choices :: Int -> Int
    choices v = sum $ map checker ys
      where
        a = assign (x,v) assignment
        checker :: Var -> Int
        checker y = length domain
          where
            cs = commonConstraints csp x y
            domain = filter (\i -> checkConstraints cs $ assign (y, i) a) $ getDomain y csp

{-
function REVISE(csp, Xi, Xj ) returns true iff we revise the domain of Xi
  revised ← false
  for each x in Di do
    if no value y in Dj allows (x ,y) to satisfy the constraint between Xi and Xj then
      delete x from Di
      revised ← true
  return revised
-}

revise :: CSP -> Var -> Var -> (CSP,Bool)
revise csp x y = foldl helper (csp, False) $ getDomain x csp
  where 
    cs = commonConstraints csp x y
    dy = getDomain y csp
    helper :: (CSP,Bool) -> Int -> (CSP,Bool)
    helper (csp, b) d = 
      if revised then (delDomainVal (x,d) csp, True)
      else (csp, b)
      where
        revised = not $ any (checkConstraints cs) $ map (\v -> assign (y,v) a) dy
        a = assign (x,d) []

{-
https://en.wikipedia.org/wiki/AC-3_algorithm

function AC-3(csp) returns false if an inconsistency is found and true otherwise
  local variables: queue, a queue of arcs, initially all the arcs in csp
  while queue is not empty do
    (Xi, Xj) ← REMOVE-FIRST(queue)
    if REVISE(csp, Xi, Xj ) then
      if size of Di = 0 then return false
      else
        for each Xk in Xi.NEIGHBORS - {Xj} do
          add (Xk, Xi) to queue
  return true
-}

ac3Check :: CSP -> [(Var,Var)] -> (CSP,Bool)
ac3Check csp [] = (csp, True)
ac3Check csp ((i,j):s) =
  if revised then
    if null di then (csp', False)
    else ac3Check csp' ns
  else ac3Check csp s
  where
     (csp', revised) = revise csp i j
     di = getDomain i csp'
     ns = s ++ (map (\k -> (k, i)) $ filter (j/=) $ allNeighboursOf i csp)

solverRecursion :: CSP -> Assignment -> (Maybe Assignment, Int)
solverRecursion csp assignment = 
  if (isComplete csp assignment) then (Just assignment,0)
  else findConsistentValue $ mcvSort acsp assignment var -- getDomain var acsp
  where 
    var = getMRVVariable assignment csp
    (acsp, passed) = ac3Check csp (map (\n -> (var, n)) $ allNeighboursOf var csp)
    findConsistentValue :: Domain -> (Maybe Assignment, Int)
    findConsistentValue [] = (Nothing,0)
    findConsistentValue (val:vs) = 
      if passed then
        if (isNothing result) then (ret,nodes+nodes'+1)
        else (result,nodes+1)
      else (ret,nodes'+1)
      where assignment' = assign (var,val) assignment
            (csp', passed) = forwardcheck acsp assignment' var
            (result,nodes) = solverRecursion csp' assignment'
            (ret,nodes') = findConsistentValue vs

solve :: CSP -> (Maybe Assignment,Int)
solve csp = solverRecursion csp []
