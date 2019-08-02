module Properties where

import Defs
-- import Solver
import Data.List
import Utils

type InstallProp = Database -> PName -> Maybe Sol -> Bool

-- for reference; may discard after implementing full install_c
install_c' :: InstallProp
install_c' _db _p Nothing = True
install_c' _db _p (Just []) = False
install_c' _db _p (Just _) = True

-- All packages (with the indicated versions) are actually available in the db
install_a :: InstallProp
install_a _ _ Nothing = True
install_a (DB db) _ (Just sol) = let db' = getNameVerList (DB db) in 
  all (`elem` db') sol

-- Any package name may only occur once in the list; in particular, it is not
-- possible to install two different versions of the same package simultaneously
install_b :: InstallProp
install_b _ _ Nothing  = True 
install_b _ _ (Just s) = allDiff s 

-- The package requested by the user is in the list
install_c :: InstallProp
install_c db p _ = 
    case p `ofSomeVersionIn` db of
        Just _  -> True  
        Nothing -> False

-- For any package in the list, 
-- all the packages it requires are also in the list
install_d :: InstallProp
install_d _ _ Nothing   = True
install_d _ _ (Just []) = True
install_d (DB db) _ (Just ((n, v):ss)) = 
  -- Find the package dependencies in the database
  case find (\p -> name p == n && ver p == v) db of
    -- Once found, extract the requirements for that package
    Just pkg -> let depsList = requirementsOf pkg in
      -- check that for all required packages
      all (\(n, lo, hi) -> 
            -- there is one package in the solution that satisfies it
            any (\(pn, v) -> (pn == n) && (lo <= v) && (v < hi)) ss) 
        depsList
    -- If no package is found in the db, then something is definitely wrong
    Nothing  -> False