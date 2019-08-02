module SolverImpl where

-- Put your solver implementation in this file.
-- Do not change the types of the following exported functions

import Defs
import Utils
import Data.List
import Parser(parseDatabase)

-- parseFile taken directly from A2. Used to read test files
parseFile :: FilePath -> IO (Either ErrMsg Database)
parseFile path = parseDatabase <$> readFile path

findDuplicatePackages :: Pkg -> [Pkg] -> [Pkg]
findDuplicatePackages p = filter f 
  where f pkg = (name pkg == name p) && (ver pkg == ver p)

findConsistentPackage :: Pkg -> [Pkg] -> Maybe Pkg
findConsistentPackage pkg [] = Just pkg
findConsistentPackage pkg (p:ps) =
  if (desc pkg == desc p) && (Just depspkg == merge depspkg depsp) 
  then findConsistentPackage pkg ps
  else Nothing
  where depspkg = deps pkg
        depsp   = deps p

-- checks if input database is consistent
-- returns a consistent db without duplicate package versions if possible
-- or returns an error if db is not consistent
isConsistentDB :: Database -> Either String Database
-- the empty db is consistent
isConsistentDB (DB [])     = Right $ DB []
-- for a non-empty db, find any duplicates of the first package p in the db
isConsistentDB (DB (p:ps)) = let duplicates = findDuplicatePackages p ps in
  -- check if p is consistent with all its duplicates
  case findConsistentPackage p duplicates of
    -- if it is, then recursively check the remaining packages
    -- with found duplicates removed
    Just pkg -> case isConsistentDB (DB (ps \\ duplicates)) of
      -- TODO: monad implementation
      -- if the recursive call succeeds, then return the db with pkg added
      Right (DB rest) -> Right $ DB (pkg:rest)
      -- otherwise, we have an error, which is passed on
      Left e          -> Left e
    -- if the package is not consistent with its duplicates, then we have error
    Nothing  -> Left "Database not consistent; a package had conflicting copies"

-- Used to sort in packages in descending order.
pkgCompare :: Pkg -> Pkg -> Ordering
pkgCompare p1 p2 = compare (ver p2) (ver p1)

sorted :: Database -> Database
sorted (DB db) = DB $ sortBy pkgCompare db

normalize :: Database -> Either String Database
normalize (DB []) = Right $ DB []
normalize db = isConsistentDB (sorted db)

reqsAndConfs :: Constrs -> (Constrs, Constrs)   
reqsAndConfs = partition (\(_, (bool, _, _) ) -> bool)

-- the package resolver
solve :: Database -> Constrs -> Sol -> [Sol]
solve db c sol = 
  -- divide constraints into required and conflicting packages
  let (reqs, conflicts) = reqsAndConfs c in
    -- then, use the solver' to solve
    solve' db reqs conflicts sol

solve' :: Database -> Constrs -> Constrs -> Sol -> [Sol]
solve' _ [] _ sol = [sol]
solve' (DB db) (r:rs) cs sol = do
  -- take any package from the database
  p <- db
  let isNotIn p ss = not (any (\(n, v) -> name p == n) ss)
  if (p `isRequiredBy` r) && (p `doesntConflictWith` cs) && p `isNotIn` sol then
    let s    = (name p, ver p)
        sol' = s:sol
        pd   = deps p
        merged = merge pd (rs ++ cs)
    in case merged of
      Just c' -> if sol' `satisfies` (r:c') then [sol']
                 else solve (DB db) c' sol'
      Nothing -> fail "Impossible." 
  else fail "Dont need this package"

isRequiredBy :: Pkg -> (PName, PConstr) -> Bool
isRequiredBy p (n, (True, lo, hi)) = name p == n && lo <= v && v < hi
                                     where v = ver p
isRequiredBy _ (_, (False, _, _)) = False

doesntConflictWith :: Pkg -> Constrs -> Bool
doesntConflictWith p c = 
  not(any (\(n, (b, lo, hi)) -> pn == n && (lo > v || v >= hi) && not b) c)
  where pn = name p
        v = ver p

install :: Database -> PName -> Maybe Sol
install (DB db) pname =
  case find (\p -> name p == pname) db of
    Just _ -> let r = (pname, (True, minV, maxV)) in
      case solve (DB db) [r] [] of
        []  -> Nothing
        sol -> Just (head sol)
    Nothing -> Nothing
  

runSolver test pkg = do 
  eitherdb <- parseFile $ "../tests/BB/testfiles/" ++ test
  case eitherdb of
    Right db -> case normalize db of
                  Right ndb -> return $ solve ndb [(P pkg, (True, minV, maxV))] []
                  _ -> fail ""
    _ -> fail ""