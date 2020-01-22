module Parse
    ( programParser
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

prsConst :: ReadP Const
prsConst = do
    string "#const"
    skipSpaces
    name <- munch1 isAlpha
    char '='
    value <- munch1 isDigit
    char '.'
    return (name, read value)

prsFact :: ReadP Fact
prsFact = do
    name <- munch1 isAlpha
    char '('
    values <- sepBy1 (skipSpaces >> munch1 isAlpha)
                     (skipSpaces >> char ',')
    string ")."
    return (name, map read values)

prsLine :: ReadP (Either Const Fact)
prsLine = do
    skipSpaces
    prsConstLeft +++ prsFactRight
    where
        prsConstLeft = prsConst >>= return . Left
        prsFactRight = prsFact >>= return . Right

prsProgram :: ReadP FactProgram
prsProgram = do
    lines <- many prsLine
    return $ assembleProgram lines

assembleProgram :: [Either Const Fact] -> FactProgram
assembleProgram = recurse $ FactProgram Map.empty Map.empty
    where
        recurse program [] = program
        recurse (FactProgram cs fs) (Left (name, value):tail) =
            recurse (FactProgram (Map.insert name value cs) fs) tail
        recurse (FactProgram cs fs) (Right (name, value):tail) =
            recurse (FactProgram cs (Map.adjust (value:) name fs)) tail

programParser :: ReadS FactProgram
programParser = readP_to_S prsProgram
