module Main where

import Graph
import Parse

main :: IO ()
main = do
    program <- fst . head . programParser <$> getContents
    mapM_ putStrLn $ toGraph program
