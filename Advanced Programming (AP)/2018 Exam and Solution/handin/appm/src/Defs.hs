module Defs where

type ErrMsg = String -- for all human-readable error messages

newtype PName = P String
  deriving (Eq, Ord, Show, Read)

data VNum = VN Int String
  deriving (Eq, Show, Read)

newtype Version = V [VNum]
  deriving (Eq, Show, Read)

minV, maxV :: Version
minV = V [VN 0 ""]        -- inclusive lower bound
maxV = V [VN 1000000 ""]  -- exclusive upper bound

type PConstr = (Bool, Version, Version) -- req'd; allowed interval [lo,hi)
type Constrs = [(PName, PConstr)]

data Pkg = Pkg {name :: PName,
                ver :: Version,
                desc :: String,
                deps :: Constrs}
  deriving (Eq, Show, Read)

newtype Database = DB [Pkg]
  deriving (Eq, Show, Read)
  
type Sol = [(PName, Version)]