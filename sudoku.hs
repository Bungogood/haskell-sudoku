import Data.Char
import Data.Maybe
import CSP

sudokuVars :: [Var]
sudokuVars = [ l : show n | l <- ['a'..'i'], n <- [1..9]]

sudokuDomains :: Domains
sudokuDomains = map (\r -> (r,[1..9])) sudokuVars

toVar :: (Int, Int) -> String
toVar (r,c) = chr (ord 'a' + r) : show (c+1)

cartProd :: t => [t] -> [t] -> [t]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

subIndex :: Int -> [Int]
subIndex n = cartProd [row..row+2] [col..col+2]
  where
    row = 3 * div (n - 1) 3
    col = 3 * mod (n - 1) 3

sudokuCsp :: CSP
sudokuCsp = CSP ("Sudoku!", sudokuDomains, map allDiffConstraint (rows ++ cols ++ subs))
  where
    rows = map (\l -> map (\n -> l : show n) [1..9]) ['a'..'i']
    cols = map (\n -> map (\l -> l : show n) ['a'..'i']) [1..9]
    subs = map (map toVar . subIndex) [1..9]


showSudokuNode csp assignment pos
  | length (getDomain pos csp) == 1 = show $ head $ getDomain pos csp
  | isNothing val = " "
  | otherwise = show $ fromJust val
  where val = lookupVar assignment pos

sudokuTable csp assignment = toTable $ cspVars csp   
  where 
    toTable l
      | l == [] = []
      | otherwise =  (map (showSudokuNode csp assignment) $ take 9 l) : (toTable $ drop 9 l)

showSudoku csp assignment = sep ++ (foldl1 foldtable $ map showRow $ sudokuTable csp assignment) ++ sep
  where 
    showRow rw = "| " ++ (foldl1 foldrow rw) ++ " |\n"
    foldrow x y = x ++ " | " ++ y
    foldtable x y = x ++ sep ++ y
    sep = "+" ++ (concat $ replicate 9 "---+") ++ "\n" 

printSudoku :: CSP -> IO()
printSudoku csp = putStr $ showSudoku csp []

trySudoku :: CSP -> (CSP -> (Maybe Assignment,Int)) -> IO()
trySudoku csp f
  | isNothing result = putStr $ "Nothing!\n Visited nodes: " ++ (show nodes) ++ "\n"
  | otherwise = putStr $ (showSudoku csp (fromJust result)) ++ "Visited nodes: " ++ (show nodes) ++ "\n"
  where (result,nodes) = f csp
