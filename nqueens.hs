import Data.Maybe
import CSP

queenVars :: Int -> [Var]
queenVars n = map (("Q"++) . show) [1..n]

queenDomains :: Int -> Domains
queenDomains n = map (\q -> (q,[1..n])) (queenVars n)

-- checks if 2 sets of coordinates are diagonal
isDiag :: (Int, Int) -> (Int, Int) -> Bool
isDiag (r1, c1) (r2, c2) = abs (r1-r2) == abs (c1-c2)

-- checks every pair of coords returns false if 2 are diagonal
checkDiag :: [(Int, Int)] -> Bool
checkDiag [] = True
checkDiag (x:xs) | any (isDiag x) xs = False
                 | otherwise = checkDiag xs

diagonalRelation :: Relation
diagonalRelation vs a = checkDiag $ map cord qs
  where
    qs :: [Var]
    qs = filter (isAssigned a) vs
    cord :: Var -> (Int, Int)
    cord q = (row q, col q)
    row :: Var -> Int
    row = read . tail
    col :: Var -> Int
    col = fromJust . lookupVar a

-- Binary constraint stating that two variables differ by exactly n.
diagonalConstraint :: Var -> Var -> Constraint
diagonalConstraint a b = CT (a ++ " and " ++ b ++ " not in the same diagonal", [a, b] , diagonalRelation)

diagcs :: [Var] -> [Constraint]
diagcs [] = []
diagcs (q:qs) = map (diagonalConstraint q) qs ++ diagcs qs

queenCsp :: Int -> CSP
queenCsp n = CSP ((show n) ++ "-Queens Problem",
             queenDomains n, allDiffConstraint qs : diagcs qs)
  where qs = queenVars n
