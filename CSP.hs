module CSP where

import Data.List
import Data.Maybe

-- https://en.wikipedia.org/wiki/Constraint_satisfaction_problem

-- *******************************************
-- ** Examples of Relations and Constraints **
-- *******************************************

-- Binary constraint stating that two variables must be different.
varsDiffConstraint :: Var -> Var -> Constraint
varsDiffConstraint a b = CT (a ++ " /= " ++ b,[a,b],varsDiff)

-- Relation that ensures two variables are different.
-- It is satisfied if either variable is unassigned.
varsDiff :: Relation
varsDiff vars assignment = not $ valsEq (lookupVar assignment a) (lookupVar assignment b)
  where a = head vars
        b = head $ tail vars


-- N-ary constraint stating that all variables in a list must have have distinct values.
allDiffConstraint :: [Var] -> Constraint
allDiffConstraint vs = CT ("All Diff: " ++ (show vs),vs,allDiff)

-- Relation that ensures a list of variables are all different.
-- Ignores unassigned variables.
allDiff :: Relation
allDiff vs a = length l == length (nub l) 
  where l = filter isJust $ map (lookupVar a) vs

-- *******************************
-- ** Variables and Assignments **
-- *******************************

-- Named variables as Strings
type Var = String

-- Single variable assignment
newtype AssignedVar = AV (Var,Int)

-- Function to prettyprint single variable assignments.
instance Show AssignedVar where
  show (AV (n,v)) = n ++ "=" ++ (show v)

-- Function to check equality of single variable assignments.
-- Defined as equality of the variable names.
instance Eq AssignedVar where
  (AV (i,_)) == (AV (j,_)) = i == j

-- Retrieve the variable name from a single variable assignment.
varName :: AssignedVar -> String
varName (AV (x,_)) = x

-- Retrieve the value of a single variable assignment.
varValue :: AssignedVar -> Int
varValue (AV (_,x)) = x
                       
-- Multiple variable assignments.
type Assignment = [AssignedVar] 

-- Retrieves the value of one of the variables in an assignment.
-- Returns Nothing if the variable is unassigned.
-- If multiple assignments of the same variable have been erroneously added, only the first one is returned.
lookupVar :: Assignment -> Var -> Maybe Int
lookupVar a x =  
  case (af) of 
    []     -> Nothing 
    av:_   -> Just (varValue av)
    where af = filter (== AV (x,0)) a
    
-- Checks whether a variable has been assigned a value.
isAssigned :: Assignment -> Var -> Bool
isAssigned a x = isJust (lookupVar a x)

-- Assigns a value to a variable. 
-- Replaces old value if variable was already assigned.
assign :: (Var,Int) -> Assignment -> Assignment
assign (v,i) a = x : (delete x a)
  where x = AV (v,i)
    
-- Checks if two values returned from lookup_var are equal. 
-- Returns False if either of them is Nothing.
valsEq :: Maybe Int -> Maybe Int -> Bool
valsEq x y 
        | isNothing x = False
        | isNothing y = False
        | otherwise = x == y



-- *******************************
-- ** Relations and Constraints **
-- *******************************

-- A Relation is defined as a function (instead of a set of acceptable tuples).
-- It is given a list of variables (scope) for the constraint and a (partial) variable assignment (tuple).
-- It returns whether the assignment (tuple) is acceptable.
type Relation = [Var] -> Assignment -> Bool

-- Instantiated constraint for a particular CSP.
-- Consists of a string for prettyprinting, the scope (list of variables), and the relation.
newtype Constraint = CT (String,[Var],Relation)

-- Prettyprinting function for Constraints.
instance Show Constraint where
  show (CT (s,_,_)) = s

-- Prettyprinting function for a list of constraints.
-- Prints every constraint on a separate line.
showConstraints :: [Constraint] -> String
showConstraints [] = ""
showConstraints (c:cs) = (show c) ++ "\n" ++ (showConstraints cs)

-- Consistency check for a single constraint on a given assignment.
checkConstraint :: Constraint -> Assignment -> Bool
checkConstraint (CT (_,vars,rel)) a = rel vars a
  
-- Consistency check for a list of constraints on a given assignment.
checkConstraints :: [Constraint] -> Assignment -> Bool
checkConstraints cs a = all (\c -> checkConstraint c a) cs

-- Returns the scope of a constraint.
scope :: Constraint -> [Var]
scope (CT (_,vars,_)) = vars

-- Returns whether a given variable is in the scope of a constraint.
isConstrained :: Var -> Constraint -> Bool  
isConstrained v c = elem v $ scope c

-- Returns the neighbours of a particular variable in a given constraint.
-- Returns an empty list if the variable is not constrained.
neighboursOf :: Var -> Constraint -> [Var]
neighboursOf v c = if (isConstrained v c) then (delete v $ scope c) else []



-- *************
-- ** Domains **
-- *************


-- Variable domains are lists of possible integers.
type Domain = [Int]

-- Adds a single value to a domain.
domainAdd :: Int -> Domain -> Domain
domainAdd i d = i:d

-- Deletes a single value from a domain.
domainDel :: Int -> Domain -> Domain
domainDel i d = delete i d


-- A list of variables attached to their respective domains.
type Domains = [(Var,Domain)]

-- Prettyprinting function for Domains list.
showDomains :: Domains -> String
showDomains [] = ""
showDomains ((v,d):ds) = v ++ " @ " ++ (show d) ++ "\n" ++ (showDomains ds)



-- *********************************************
-- ** Constraint Satisfaction Problems (CSPs) **
-- *********************************************


-- CSPs consist of a name, an (initial) Domains list, and a list of Constraints.
newtype CSP = CSP (String,Domains,[Constraint])

-- Prettyprinting function for CSPs.
instance Show CSP where
  show (CSP (s,d,c)) = "CSP: " ++ s 
                       ++ "\n\nDomains:\n--------\n" ++ (showDomains d) 
                       ++ "\n\nConstraints:\n------------\n" ++ (showConstraints c)
                       

-----------------------------
-- Basic getter functions. --
-----------------------------

-- Retrieve the list of variables defined in a CSP.  
cspVars :: CSP -> [Var]
cspVars (CSP (_,d,_)) = map fst d

-- Retrieve the Domains list of a CSP.
cspDomains :: CSP -> Domains
cspDomains (CSP (_,d,_)) = d

-- Retrieve the list of constraints of a CSP.
cspConstraints :: CSP -> [Constraint]    
cspConstraints (CSP (_,_,cs)) = cs


-----------------------
-- Domain functions. --
-----------------------

-- Set a new Domains list for the CSP.
setDomains :: Domains -> CSP -> CSP
setDomains d' (CSP (s,_,cs)) = (CSP (s,d',cs))

-- Retrieves the domain for a particular variable in a CSP.
getDomain :: Var -> CSP -> Domain
getDomain v csp
  | isJust r = fromJust r
  | otherwise = []
    where r = lookup v $ cspDomains csp

-- Updates the domain of a given variable in a Domains list by applying a function to it.
updateDomain :: (Domain -> Domain) -> Var -> Domains -> Domains
updateDomain _ [] _ = []
updateDomain f var ((v,vals):dm)
             | v == var = (var,f vals):dm 
             | otherwise = (v,vals):(updateDomain f var dm)

-- Updates the domain of a given variable in a CSP by applying a function to it.
updateDomains :: (Domain -> Domain) -> Var -> CSP -> CSP
updateDomains f v csp = flip setDomains csp $ updateDomain f v $ cspDomains csp

-- Sets a new domain for a variable in a CSP.
setDomain :: (Var,Domain) -> CSP -> CSP 
setDomain (v,dom) csp = updateDomains (\d -> dom) v csp
                                                 
-- Adds a value to the domain of a variable in a CSP.
addDomainVal :: (Var,Int) -> CSP -> CSP
addDomainVal (v,i) csp = updateDomains (domainAdd i) v csp

-- Deletes a value from the domain of a variable in a CSP.
delDomainVal :: (Var,Int) -> CSP -> CSP
delDomainVal (v,i) csp = updateDomains (domainDel i) v csp



-------------------------
-- Variable functions. --
-------------------------

-- Returns the first variable from a CSP that is unassigned in a given partial assignment.
firstUnassignedVar :: Assignment -> CSP -> Var
firstUnassignedVar assignment csp = head $ filter (\x -> not $ isAssigned assignment x) $ cspVars csp 

-- Returns the list of constraints that constrain a given variable in a CSP.
constraintsOf :: Var -> CSP -> [Constraint]
constraintsOf v csp = filter (isConstrained v) $ cspConstraints csp

-- Returns a list of all neighbours from all constraints for a given variable in a CSP.
allNeighboursOf :: Var -> CSP -> [Var]
allNeighboursOf v csp = foldr (++) [] $ map (neighboursOf v) $ cspConstraints csp
                                
-- Returns a list of constraints that involve both of two given variables.
commonConstraints :: CSP -> Var -> Var -> [Constraint]
commonConstraints csp a b = filter (isConstrained a) $ constraintsOf b csp


---------------------------
-- Assignment functions. --
---------------------------

-- Checks if a given assignment is complete for a CSP.
isComplete :: CSP -> Assignment -> Bool
isComplete csp assignment = all (isAssigned assignment)$ cspVars csp

-- Checks if an assignment is consistent with respect to the constraints of a CSP.
isConsistent :: CSP -> Assignment -> Bool
isConsistent csp assignment = all (\c -> checkConstraint c assignment) $ cspConstraints csp

-- Checks if a suggested value for a variable is consistent w.r.t a partial assignment and a CSP.
isConsistentValue :: CSP -> Assignment -> (Var,Int) -> Bool
isConsistentValue csp assignment a = isConsistent csp $ assign a assignment
