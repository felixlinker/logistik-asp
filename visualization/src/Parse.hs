module Parse
    ( programmParser
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

prsProgramm :: ReadP FactProgram
prsProgramm = do
    lines <- many prsLine
    return $ assembleProgramm lines

assembleProgramm :: [Either Const Fact] -> FactProgram
assembleProgramm = recurse $ FactProgram Map.empty Map.empty
    where
        recurse programm [] = programm
        recurse (FactProgram cs fs) (Left (name, value):tail) =
            recurse (FactProgram (Map.insert name value cs) fs) tail
        recurse (FactProgram cs fs) (Right (name, value):tail) =
            recurse (FactProgram cs (Map.adjust (value:) name fs)) tail

programmParser :: ReadS FactProgram
programmParser = readP_to_S prsProgramm
