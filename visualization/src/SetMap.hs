module SetMap
    ( SetMap
    , insertMaybe
    , nestedInsert )
    where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type SetMap a = Map.Map Int (Set.Set a)

insertMaybe :: Ord a => a -> Maybe (Set.Set a) -> Maybe (Set.Set a)
insertMaybe v (Just s) = Just $ Set.insert v s
insertMaybe v Nothing = Just $ Set.singleton v

nestedInsert :: Ord a => Int
                      -> Int
                      -> a
                      -> Map.Map Int (SetMap a)
                      -> Map.Map Int (SetMap a)
nestedInsert k1 k2 v = Map.alter (nestedAlter (insertMaybe v) k2) k1
    where
        nestedAlter :: Ord a => (Maybe (Set.Set a) -> Maybe (Set.Set a))
                             -> Int
                             -> Maybe (SetMap a)
                             -> Maybe (SetMap a)
        nestedAlter f k2 (Just m) = Just $ Map.alter f k2 m
        nestedAlter f k2 Nothing = (f Nothing) >>= (Just . Map.singleton k2)
