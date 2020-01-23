module Parse
    ( Name
    , Value
    , Const
    , Fact
    , FactProgram (..)
    , programParser
    ) where

import Text.ParserCombinators.ReadP
import Data.Char
import SetMap
import Data.Maybe
import Control.Monad
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type Name = String
type Value = Int

type Const = (Name, Value)
type Fact = (Name, [Value])

data FactProgram = FactProgram
    { consts :: Map.Map Name Value
    , facts :: Map.Map Name (Set.Set [Value])
    }

isVarChar :: Char -> Bool
isVarChar = (||) <$> isAlphaNum <*> (== '_')

prsConst :: ReadP Const
prsConst = do
    string "#const"
    skipSpaces
    name <- munch1 isVarChar
    char '='
    value <- munch1 isAlphaNum
    char '.'
    return (name, read value)

prsFact :: ReadP Fact
prsFact = do
    name <- munch1 isVarChar
    char '('
    values <- sepBy1 (skipSpaces >> munch1 isAlphaNum)
                     (skipSpaces >> char ',')
    string ")."
    return (name, map read values)

skipCost :: ReadP (Maybe a)
skipCost = do
    string "COST"
    skipSpaces
    skipMany1 $ satisfy isDigit
    return Nothing

skipMeta :: ReadP (Maybe a)
skipMeta = do
    skipSpaces
    skipAnswer <++ skipCost <++ skipOpt
  where skipAnswer :: ReadP (Maybe a)
        skipAnswer = string "ANSWER" >> return Nothing
        skipOpt :: ReadP (Maybe a)
        skipOpt = string "OPTIMUM" >> return Nothing

skipComment :: ReadP (Maybe a)
skipComment = do
    skipSpaces
    char '%'
    skipMany $ satisfy isPrint
    return Nothing

prsLine :: ReadP (Maybe (Either Const Fact))
prsLine = do
    skipSpaces
    skipComment <++ skipMeta <++ prsFactRight <++ prsConstLeft
    where
        prsConstLeft = Just . Left <$> prsConst
        prsFactRight = Just . Right <$> prsFact

prsProgram :: ReadP [Either Const Fact]
prsProgram = do
    lines <- catMaybes <$> many prsLine
    skipSpaces
    eof
    return lines

assembleProgram :: [Either Const Fact] -> FactProgram
assembleProgram = recurse $ FactProgram Map.empty Map.empty
    where
        recurse program [] = program
        recurse (FactProgram cs fs) (Left (name, value):tail) =
            recurse (FactProgram (Map.insert name value cs) fs) tail
        recurse (FactProgram cs fs) (Right (name, value):tail) =
            recurse (FactProgram cs (Map.alter (insertMaybe value) name fs)) tail

programParser :: ReadS FactProgram
programParser = readP_to_S (assembleProgram <$> prsProgram)
