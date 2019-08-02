module Utils where

-- Any auxiliary code to be shared by Parser, Solver, or tests
-- should be placed here.

import Defs
import Data.List
import Control.Monad


instance Ord Version where
  -- if the first version is empty, then it is always less than or equal
  (<=) (V []) _ = True
  -- similarly, if the first is non-empty, but the second is empty,
  -- the first is largest
  (<=) (V (_:_)) (V []) = False
  -- otherwise, we inspect the elements of the version
  (<=) (V (VN i1 s1 : xs)) (V (VN i2 s2 : ys))
    -- if the numeric parts are distinct, then we can simply compare those
    | i1 /= i2 = i1 < i2
    -- otherwise, we have to compare suffixes. If they are equal as well,
    -- we have to look at the remainder of the version number.
    | s1 == s2 = V xs <= V ys
    -- if the suffixes are distinct, but same length, we can simply compare them
    | length s1 == length s2 = s1 <= s2
    -- otherwise, we know that the shortest is always smaller
    | otherwise = length s1 < length s2

{-- 
merge c1 c2 should conjoin the constraints, producing a new, well-formed
constraints list. If the resulting set would be inherently unsatisfiable, i.e.
contain a constraint saying that some package is both required and must have a
version number belonging to an empty interval, merge should return nothing.
--}
merge :: Constrs -> Constrs -> Maybe Constrs
-- if either of the given constraints are empty, return the nonempty input.
merge c1 [] = Just c1
merge [] c2 = Just c2
merge c1 c2 = foldM mergeFolder c2 c1

mergeFolder :: Constrs -> (PName, PConstr) -> Maybe Constrs
mergeFolder cs (n1, c1) =
  -- if name is already in accumulator, then check it
  case matches of
    -- the package is not in the second list, and we can safely add it
    [] -> return $ (n1, c1) : cs  
    -- the package is in both lists, so we have to check for feasibility
    [(_, c2)] ->
      -- extract the bounds and required bool from both constraints
      let ((b1, lo1, hi1), (b2, lo2, hi2)) = (c1, c2)
          -- find lower and upper bounds
          lower = max lo1 lo2 :: Version 
          upper = min hi1 hi2 :: Version 
      in
        -- if the bounds denote an empty interval, then we have nothing
        -- that is, if at least one of them is required
        if lower >= upper && (b1 || b2) then Nothing 
        -- otherwise, we return the new bounds for the package, plus the rest
        else return $ (n1, (b1 || b2, lower, upper)) : remainder
    _ -> Nothing -- Any package should be mentioned at most once in the list

  -- we split the second input into packages with same name, and the others
  where (matches, remainder) = partition (\(n2, _) -> n2 == n1) cs


-- Helper functions for the PROPERTIES PART

getNameVerList :: Database -> [(PName, Version)]
getNameVerList (DB db) = map (\p -> (name p, ver p)) db

ofSomeVersionIn :: PName -> Database -> Maybe Version
ofSomeVersionIn pname (DB db) = 
  case find (\p -> name p == pname) db of
    Just pkg -> Just (ver pkg)
    Nothing -> Nothing 


requirementsOf :: Pkg -> [(PName, Version, Version)]
requirementsOf pkg = 
  let reqs = filter (\(_, (b, _, _)) -> b) (deps pkg) in
    map (\(pn, (_, lo, hi)) -> (pn, lo, hi)) reqs    

-- Returns True iff all package names in the input list are unique
allDiff :: [(PName, Version)] -> Bool
allDiff [] = True
allDiff ((n, _):ss) = not (any (\(n', _) -> n == n') ss) && allDiff ss

-- checks if a solution satifies all constraints
satisfies :: Sol -> Constrs -> Bool
satisfies _ [] = True
satisfies sol cs = foldl folder True cs
  where folder acc (cn, (requires, lo, hi)) =
          if requires 
          then acc && any (\(sn, sv) -> cn == sn && lo <= sv && sv < hi) sol
          else acc && all (\(sn, sv) -> cn /= sn || (lo <= sv && sv < hi)) sol

-- goes through all the constraints in the input and merges them
-- this basically checks that the conjoined constraints are satisfiable
sanityCheck :: Constrs -> Maybe Constrs
sanityCheck [] = Just []
sanityCheck (c:cs) = merge [c] cs

isWellFormed :: [Constrs] -> Maybe Constrs
isWellFormed constraints = do
  collected <- mapM sanityCheck constraints
  foldM merge [] collected