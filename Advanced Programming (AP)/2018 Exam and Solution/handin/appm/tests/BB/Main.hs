module Main where

-- Put your black-box tests in this file

import Defs
import Parser (parseDatabase)
import Solver (install, normalize)
import Utils

import Test.Tasty
import Test.Tasty.HUnit

-- parseFile taken directly from A2. Used to read test files
parseFile :: FilePath -> IO (Either ErrMsg Database)
parseFile path = parseDatabase <$> readFile path

-- where 
path = "tests/BB/testfiles/"

-- Similarly, many of these structural procedures are taken directly
-- from earlier test files, and in some cases generalized
runTest pAct pExp = do
  act <- parseFile $ path ++ pAct
  exp <- fmap read $ readFile $ path ++ pExp
  act @?= Right exp

tests = testGroup "Unit tests"
  [ testGroup "Merge tests" 
      [ testCase "examExample1" $ c1 `merge` c2 @?= Just c3
      , testCase "examExample2" $ c4 `merge` c5 @?= Nothing 
      , testCase "examExample3" $ c6 `merge` c7 @?= Just c8
      , testCase "forumExample1" $ c9 `merge` c10 @?= Just c11
      , testCase "forumExample2" $ c12 `merge` c13 @?= Nothing
      , testCase "req and conf" $ c14 `merge` c15 @?= Just c16
      ]
    
  , testGroup "Parser tests"
      [ testCase "tiny" $ parseDatabase "package {name foo}" @?= Right db
      , testCase "intro"                      $ runTest "test1" "test1e"     
      , testCase "intro2"                     $ runTest "test2" "test2e"     
      , testCase "case insensitive keywords"  $ runTest "test3" "test3e"     
      , testCase "jumbled clause order"       $ runTest "test4" "test4e"     
      , testCase "no description"             $ runTest "test5" "test5e"     
      , testCase "no version"                 $ runTest "test6" "test6e"     
      , testCase "no ending semi colon"       $ runTest "test7" "test7e"     
      , testCase "ending semi colon"          $ runTest "test8" "test8e"     
      , testCase "simple name"                $ runTest "test9" "test9e"     
      , testCase "general name"               $ runTest "test10" "test10e"     
      , testCase "version suffix"             $ runTest "test11" "test11e"     
      ]

  , testGroup "Solver tests"
      [ testCase "tiny"   $ install db pname @?= Just [(pname, ver)]
      , testCase "intro"  $ do
          db1 <- fmap read $ readFile $ path ++ "test1e"
          case normalize db1 of
            Right ndb -> install ndb (P "foo") @?= e1
            _ -> fail ".."
          
      , testCase "intro2" $ do
          db2 <- fmap read $ readFile $ path ++ "test2e"
          case normalize db2 of
            Right ndb -> install ndb (P "foo") @?= e2
            _ -> fail ".."  
      , testCase "large case1" $ do
          db3 <- fmap read $ readFile $ path ++ "test12e"
          case normalize db3 of
            Right ndb -> install ndb (P "chrome") @?= e3
            _ -> fail ".."
      , testCase "large case2" $ do
          eitherdb4 <- parseFile $ path ++ "test13"
          case eitherdb4 of
            Right db4 -> case normalize db4 of
                           Right ndb -> install ndb (P "a") @?= e4
                           _ -> fail ""
            _ -> fail ""
      , testCase "Small special case" $ do
          eitherdb4 <- parseFile $ path ++ "test14"
          case eitherdb4 of
            Right db4 -> case normalize db4 of
                           Right ndb -> install ndb (P "a") @?= Nothing
                           _ -> fail ""
            _ -> fail ""
      ] 
  ]
  where
    pname = P "foo"
    ver = V [VN 1 ""]
    db = DB [Pkg pname ver "" []]
    e1 = Just [ (P "bar",V [VN 2 "",VN 1 ""])
              , (P "foo",V [VN 2 "",VN 3 ""]) ]
    e2 = Just [ (P "baz",V [VN 6 "",VN 1 "",VN 2 ""])
              , (P "bar",V [VN 1 "",VN 0 ""])
              , (P "foo",V [VN 2 "",VN 3 ""]) ]
    e3 = Just [ (P "foo",V [VN 2 "",VN 3 ""])
              , (P "baz",V [VN 6 "",VN 1 "",VN 2 ""])
              , (P "bar",V [VN 5 "ff",VN 32 ""])
              , (P "chrome",V [VN 3 "",VN 0 "aa"])]
    e4 = Just [ (P "b",V [VN 5 "",VN 0 ""])
              , (P "c",V [VN 4 "",VN 0 ""])
              , (P "d",V [VN 5 "",VN 0 ""])
              , (P "a",V [VN 4 "",VN 0 ""])]
    c1 = [ (P "bar", (True,  V [VN 1 ""], V [VN 1000000 ""]))
         , (P "foo", (False, V [VN 2 "", VN 3 ""], V [VN 4 ""]))
         , (P "baz", (True,  V [VN 1 "", VN 3 ""], V [VN 5 ""]))
         ]
    c2 = [ (P "bar", (True,  V [VN 1 "a"], V [VN 2 ""]))
         , (P "foo", (True, V [VN 2 "", VN 4 ""], V [VN 2 "", VN 7 ""]))
         , (P "baz", (False,  V [VN 2 "", VN 0 ""], V [VN 3 "", VN 5 ""]))
         ]
    c3 = [ (P "baz", (True,  V [VN 2 "", VN 0 ""], V [VN 3 "", VN 5 ""]))
         , (P "foo", (True,  V [VN 2 "", VN 4 ""], V [VN 2 "", VN 7 ""]))
         , (P "bar", (True,  V [VN 1 "a"], V [VN 2 ""]))
         ]
    c4 = [ (P "foo", (True,  V [VN 5 "", VN 0 ""], maxV)) ]
    c5 = [ (P "foo", (True,  minV, V [VN 2 "", VN 0 ""])) ]
    c6 = [ (P "foo", (False,  V [VN 3 "", VN 4 "", VN 4 ""], V [VN 3 "", VN 4 "", VN 2 ""])) ]
    c7 = [ (P "bar", (True,  minV, V [VN 2 "", VN 0 ""])) ]
    c8 = [(P "foo",(False,V [VN 3 "",VN 4 "",VN 4 ""],V [VN 3 "",VN 4 "",VN 2 ""])),(P "bar",(True,V [VN 0 ""],V [VN 2 "",VN 0 ""]))]
    c9 = [(P "foo", (True, V [VN 2 ""], V [VN 8 ""]))]
    c10 = [(P "foo", (False, V [VN 4 ""], V [VN 6 ""]))]
    c11 = [(P "foo", (True, V [VN 4 ""], V [VN 6 ""]))]
    c12 = [(P "foo", (True, V [VN 6 ""], V [VN 8 ""]))]
    c13 = [(P "foo", (True, V [VN 4 ""], V [VN 6 ""]))]
    c14 = [(P "e", (False, minV, maxV))]
    c15 = [(P "c", (True, minV, maxV))]
    c16 = [(P "e", (False, minV, maxV)), (P "c", (True, minV, maxV))]

main = defaultMain tests