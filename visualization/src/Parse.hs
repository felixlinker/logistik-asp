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
import qualified Data.Map as Map

type Name = String
type Value = Int

type Const = (Name, Value)
type Fact = (Name, [Value])

data FactProgram = FactProgram
    { consts :: Map.Map Name Value
    , facts :: Map.Map Name [[Value]]
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

prsLine :: ReadP (Either Const Fact)
prsLine = do
    skipSpaces
    prsFactRight <++ prsConstLeft
    where
        prsConstLeft = Left <$> prsConst
        prsFactRight = Right <$> prsFact

prsProgram :: ReadP [Either Const Fact]
prsProgram = do
    lines <- many prsLine
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
            recurse (FactProgram cs (Map.adjust (value:) name fs)) tail

programParser :: ReadS FactProgram
programParser = readP_to_S (assembleProgram <$> prsProgram)
