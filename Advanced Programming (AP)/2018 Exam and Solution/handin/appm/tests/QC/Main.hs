module Main where

import Defs
import Properties
import Solver (install, normalize)
import Utils (isWellFormed)

import Test.Tasty
import Test.Tasty.QuickCheck
import Data.List

instance Arbitrary PName where
  arbitrary = nameGenerator
  
names = [ return $ P "Package01"
        , return $ P "Package02"
        , return $ P "Package03"
        , return $ P "Package04"
        , return $ P "Package05"
        , return $ P "Package06"
        , return $ P "Package07"
        , return $ P "Package08"
        , return $ P "Package09"
        , return $ P "Package10"
        , return $ P "Package11"
        , return $ P "Package12"
        , return $ P "Package13"
        , return $ P "Package14"
        , return $ P "Package15"
        , return $ P "Package16"
        ]

-- nameGenerator = oneof [simpleNameGenerator, generalNameGenerator]
nameGenerator = oneof names 

simpleNameGenerator = simpleName
generalNameGenerator = generalName

-- Simple name generator
asciiLetter    = elements $ ['a'..'z'] ++ ['A'..'Z']
alphaNumHyphen = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['-']
digits         = elements ['0' .. '9']

simpleName = do 
  h <- asciiLetter
  n <- choose (0, 20)
  s <- vectorOf n alphaNumHyphen
  return $ P $ h : s

fieldsGenerator = do
  n <- choose (1,5)
  vectorOf n fieldGenerator

fieldGenerator = do
  n <- choose (1,3)
  numeral <- vectorOf n digits
  suffix  <- oneof [return "", vectorOf n asciiLetter]
  return $ VN (read numeral) suffix

versionGenerator = V <$> fieldsGenerator

descriptionGenerator = do
  n <- choose (0, 10)
  vectorOf n alphaNumHyphen

constraintsGenerator = do
  n <- choose (0, 2)
  vectorOf n constraintGen

genConstraintWithName n = do
  name <- n
  bool <- elements [True, False]
  v1 <- oneof [versionGenerator, return minV]
  v2 <- oneof [versionGenerator, return maxV]
  if v1 <= v2 then return (name, (bool, v1, v2))
  else return (name, (bool, v2, v1))

constraintGen = do
  name <- nameGenerator
  bool <- elements [True, False]
  v1 <- oneof [versionGenerator, return minV]
  v2 <- oneof [versionGenerator, return maxV]
  if v1 <= v2 then return (name, (bool, v1, v2))
  else return (name, (bool, v2, v1))

instance Arbitrary Database where
  arbitrary = databaseGenerator
  -- the shrink simply takes all combinations of the db with one package removed
  shrink (DB db) = do
    p <- db
    return $ DB $ delete p db

databaseGenerator = do
  n <- choose (0,16)
  packages <- vectorOf n packageGenerator
  return $ DB packages

satisfiableConstraints p = 
  case isWellFormed [(deps p)] of Just _ -> True; _ -> False

pName p = let nms = filter (\(n, _) -> name p == n) (deps p) in length nms == 0 

isWellFormedBool p = satisfiableConstraints p && pName p

packageGenerator = unsafePackageGen `suchThat` isWellFormedBool

unsafePackageGen = do
  name    <- nameGenerator
  version <- versionGenerator
  desc    <- descriptionGenerator
  Pkg name version desc <$> constraintsGenerator

genPackWithName :: PName -> Gen Pkg
genPackWithName pn = do
  ver <- versionGenerator
  des <- descriptionGenerator
  Pkg pn ver des <$> constraintsGenerator

-----------------------------------
-- Actual properties for testing 
-----------------------------------

prop_install_a db p = 
  case normalize db of
    Right d -> install_a d p (install db p)
    Left _ -> True

prop_install_b db p = install_b db p (install db p)

prop_install_c (DB db) p = do
  -- for whatever package name p that was generated, make a package for it
  pkg <- genPackWithName p
  -- and put it in the database before installing it
  let db' = pkg:db in case normalize (DB db') of
    Right d -> return $ install_c d p (install d p)
    Left  _ -> return $ True

prop_install_d db p = install_d db p (install db p)
  
tests = testGroup "QC tests" [ testProperty "Prop (a)" prop_install_a
                             , testProperty "Prop (b)" prop_install_b
                             , testProperty "Prop (c)" prop_install_c
                             , testProperty "Prop (d)" prop_install_d
                             ]
          
main = defaultMain tests

generalName = do
  n <- choose (0, 20)
  s <- vectorOf n (oneof [vectorOf 2 alphaNumHyphen, return "\"\""])
  return $ P $ ['\"'] ++ concat s ++ ['\"']