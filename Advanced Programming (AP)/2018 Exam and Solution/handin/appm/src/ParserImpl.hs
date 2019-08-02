-- hlint told me to use lambda cases, so that's why this is included 
{-# LANGUAGE LambdaCase #-}

module ParserImpl where

-- put your parser in this file. Do not change the types of the following
-- exported functions

import Defs
import Utils

import Data.Char
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim hiding (token)
import Text.Parsec.String
import Control.Monad


-- The structure of both parseVersion and parseDatabase is very close to what 
-- we did in assignment A2 for the substript parser. 
parseVersion :: String -> Either ErrMsg Version
parseVersion s = case parse versionParser "" s of
  Left  err     -> Left $ show err
  Right version -> Right version

parseDatabase :: String -> Either ErrMsg Database
parseDatabase s = case parse databaseParser "" s of
  Left  err -> Left $ show err
  Right db  -> Right db

databaseParser :: Parser Database
databaseParser = token $ do
  pSpaceComments
  packages <- many packageParser
  eof
  return $ DB packages

-- data used for parsing clauses
data Clause = NC PName
            | VC Version
            | DC String
            | CC Constrs
            | Epsilon

-- really ugly, sorry! but I had trouble making it work with (<|>)
clauseParser :: Parser Clause
clauseParser = token $ do
  -- maybe parse a name clause
  name <- optionMaybe (do keywordParser "name"
                          packageNameParser)
  case name of
    Just n -> return $ NC n
    Nothing -> do
      -- maybe parse a version clause
      ver <- optionMaybe (do keywordParser "version"
                             versionParser)
      case ver of
        Just v -> return $ VC v
        Nothing -> do
          -- maybe parse a requied constraint
          req <- optionMaybe requiredParser

          case req of
            Just r -> return $ CC r
            Nothing -> do
              -- maybe parse a conflict constraint
              con <- optionMaybe conflictsParser

              case con of 
                Just c -> return $ CC c
                Nothing -> do 
                  -- maybe parse a description
                  descr <- optionMaybe (do keywordParser "description" 
                                           stringParser)
                  case descr of
                    Just d -> return $ DC d
                    -- if none of these work, then we have the empty clause
                    Nothing -> return Epsilon




cleanUp :: [ Clause ] -> Maybe (PName, Version, String, Constrs)
cleanUp []      = Nothing
cleanUp clauses =
  -- filter out all the disticts clauses from the Clause data type
  let nameClauses = filter (\case NC _ -> True; _ -> False) clauses
      versClauses = filter (\case VC _ -> True; _ -> False) clauses
      descClauses = filter (\case DC _ -> True; _ -> False) clauses
      consClauses = filter (\case CC _ -> True; _ -> False) clauses
      -- we need to check the number of some of these clauses
      ln = length nameClauses
      lv = length versClauses
      ld = length descClauses

      -- we're not using a monad for this, so have to unpack
      unpackedConstraints = 
        map (\case CC c -> c; _ -> error "") consClauses

      -- check that there is at most one specified version
      version = if lv == 0 then V [VN 1 ""] 
                else case head versClauses of VC v -> v; _ -> error ""
      
      -- check the description
      description = if ld == 0 then "" 
                    else case head descClauses of DC s -> s; _ -> error ""

  -- ok, so we have a problem if there is not exactly one name,
  -- or if we have more than one version,
  -- or if we have more than one description
  in if ln /= 1 || lv > 1 || ld > 1 then Nothing
     else let name = case head nameClauses of NC n -> n; _ -> error ""
          -- check that constraints are well-formed
          in case isWellFormed unpackedConstraints of
              -- and return them in order if possible
              Just nice -> Just (name, version, description, nice)
              Nothing   -> Nothing

-- parses a package
packageParser :: Parser Pkg
packageParser = token $ do
  keywordParser "package"
  token $ char '{'
  clauses <- sepBy1 clauseParser (token (char ';'))
  token $ char '}'
  -- check that parsed clauses are well-formed
  case cleanUp clauses of
    Just (name, ver, descr, constr) -> return $ Pkg name ver descr constr
    Nothing -> fail "Clauses are not semantically well-formed."

requiredParser :: Parser [(PName, PConstr)]
requiredParser = token $ do
  keywordParser "requires"
  sepBy1 (singleConstParser True ">=" "<") (token (char ','))

boundParser :: String -> Parser Version
boundParser op = token $ do
  token $ string op
  versionParser

conflictsParser :: Parser [(PName, PConstr)]
conflictsParser = token $ do
  keywordParser "conflicts"
  sepBy1 (singleConstParser False "<" ">=") (token (char ','))

singleConstParser :: Bool -> String -> String -> Parser (PName, PConstr)
singleConstParser bool op1 op2 = token $ do
  name <- packageNameParser
  low  <- option minV (boundParser op1)
  high <- option maxV (boundParser op2)
  if low <= high then return (name, (bool, low, high)) 
  else fail "version interval is empty"

-- Parses the version type
versionParser :: Parser Version
versionParser = token $ do
  -- parse initial whitespace and comments
  pSpaceComments
  -- parse at least one version number, each separated by '.'
  versionNumbers <- sepBy1 singleVNumParser (char '.')  
  -- return as a version type
  return $ V versionNumbers

singleVNumParser :: Parser VNum
singleVNumParser = do
   -- parse at least one digit for the number of the version
  numbers <- many1 digit
  -- parse an optional suffix of at most 4 chars
  optionalSuffix <- many (satisfy isAsciiLower)
  -- check constraints on number range and suffic length before returning VNum
  if (read numbers < 1000000) || length optionalSuffix < 5 
  then return $ VN (read numbers) optionalSuffix
  else error "Number is greater than 999 999 or suffix is too long."

isAlphaNumOrHyphen :: Char -> Bool
isAlphaNumOrHyphen c = isAlphaNum c || (c == '-')

-- I added hyphen to they keyword parser from A2, so that reserved keywords
-- are not followed by either alphanum or a hyphen (ver 1.0 of description) 
-- I also added case insensitiveness
-- Otherwise it is pretty much the same function in case you wonder
keywordParser :: String -> Parser ()
keywordParser ""   = token $ notFollowedBy (satisfy isAlphaNumOrHyphen)
keywordParser (c:cs) = do
  oneOf [toUpper c, toLower c]
  keywordParser cs

asciiLetters = ['A' .. 'Z'] ++ ['a' .. 'z']

lettersAndDigitsParser :: Parser String
lettersAndDigitsParser = many1 (oneOf asciiLetters <|> digit)

hyphenParser :: Parser String
hyphenParser = do
  hyp <- char '-'
  lads <- lettersAndDigitsParser
  return $ hyp : lads

-- a parser for the simple package name
simplePackageNameParser :: Parser String
simplePackageNameParser = token $ do
  head  <- oneOf asciiLetters
  slack <- many (oneOf asciiLetters)
  rest  <- many hyphenParser
  return $ head : slack ++ concat rest

stringParser :: Parser String
stringParser = token $ do
  char '\"'
  stringContent <- stringContentParser
  char '\"'
  return stringContent

-- a parser for the general package name
generalPackageNameParser :: Parser String
generalPackageNameParser = stringParser

isValidStringChar :: Char -> Bool
isValidStringChar c = isAscii c && (c /= '"')

-- Helper functions for string content parser:
-- asciiNotQuoteParser parses one or more of all ascii chars that are not '\"'
asciiNotQuoteParser = many1 $ satisfy isValidStringChar
-- quoteParser parses one or more of the string "\"\""
quoteParser = do 
  doublequotes <- many1 $ string ['\"', '\"']
  -- we only want to return one quote for each pair of quotes we parsed
  return ['\"' | _ <- [1 .. length doublequotes]]

stringContentParser :: Parser String
stringContentParser = do
  a <- many (asciiNotQuoteParser <|> try quoteParser)
  return $ concat a

-- This is the top package name parser
packageNameParser :: Parser PName
packageNameParser = token $ do
  -- either parse a simple or a general name and return it as a PName
  name <- simplePackageNameParser <|> generalPackageNameParser
  return $ P name

-- THE FOLLOWING CODE IS DIRECTLY TAKEN FROM OUR HANDIN OF ASSIGNMENT A2
-- It is used for parsing comments, whitespace, tokens, etc., and so is almost
-- completely similar. Comments are now starting with '--' instead of '//'

-- One-line comments
pComments :: Parser ()
pComments = do
  string "--"
  many $ noneOf "\n"
  spaces
  return ()

-- Spaces and comments
pSpaceComments :: Parser ()
pSpaceComments = do
  spaces
  many pComments
  return ()

-- Parse the parser followed by whitespace/comments
token :: Parser a -> Parser a
token p = do
  r <- p
  pSpaceComments
  return r

-- Parse the string followed by whitespace/comments
symbol :: String -> Parser String
symbol s = token $ string s