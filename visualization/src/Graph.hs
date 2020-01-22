module Graph
    ( toGraph
    )
where

import           Parse
import           Text.Printf
import           SetMap
import           Data.Map.Merge.Strict
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set

class EdgeYielder a where
    toEdge :: Int -> a -> String

toGraph :: FactProgram -> [String]
toGraph p =
    let drives = driveSchedules p
        loads  = loadSchedules p
        starts = truckStarts p
        zipped = merge (mapMissing (\_ -> map Right))
                       (mapMissing (\_ -> map Left))
                       (zipWithMatched (const mergeActs))
                       loads
                       drives
    in  Map.foldr (++) []
            $ Map.mapWithKey (\k -> toEdgeRec 0) zipped
  where
    mergeActs :: [Loads] -> [Drives] -> [Either Drives Loads]
    mergeActs (l : ls) =
        (Right l :) . concat . zipWith (\a b -> [Right a, Left b]) ls
    toEdgeRec :: Int -> [Either Drives Loads] -> [String]
    toEdgeRec _ []               = []
    toEdgeRec s (Left  d : tail) = toEdge s d : toEdgeRec (num $ dstep d) tail
    toEdgeRec s (Right l : tail) = toEdge s l : toEdgeRec s tail

data Place = Place { pid :: Int
                   , pday :: Int } deriving (Eq, Ord)
instance Show Place where
    show (Place i d) = printf "n%d_%d" i d

data Edge = Edge { start :: Place
                 , end :: Place } deriving (Eq, Ord)
instance Show Edge where
    show (Edge f t) = show f ++ " -> " ++ show t ++ ";"
fromCost :: [Int] -> Edge
fromCost (f : t : _) = Edge (Place f 0) (Place t 0)

roadGraph :: FactProgram -> Set.Set Edge
roadGraph program = Set.map fromCost $ facts program Map.! "cost"

setDays :: Int -> Set.Set Edge -> Set.Set Edge
setDays d = Set.map $ \(Edge f t) -> Edge (f { pday = d }) (t { pday = d })

data Step = Step { day :: Int
                 , num :: Int } deriving (Eq, Ord)

data Drives = Drives { dstep :: Step
                     , to :: Int } deriving (Eq, Ord)
instance EdgeYielder Drives where
    toEdge start (Drives (Step d s) t) =
        let startNode = show $ Place start d
            endNode   = show $ Place t d
        in  printf "%s -> %s [label=\"@%d\"];" startNode endNode s

fromList :: [Int] -> Drives
fromList (to : step : day : _) = Drives (Step day step) to

driveSchedules :: FactProgram -> Map.Map Int [Drives]
driveSchedules program =
    Map.map Set.toAscList . foldr update Map.empty $ facts program Map.! "drive"
    where update (tr : vals) = Map.alter (insertMaybe $ fromList vals) tr

data Loads = Loads { lstep :: Step
                   , unload :: Int
                   , load :: Int } deriving (Eq, Ord)
instance Semigroup Loads where
    (Loads s1 u1 l1) <> (Loads s2 u2 l2) =
        Loads (max s1 s2) (u1 + u2) (l1 + l2)
instance EdgeYielder Loads where
    toEdge start (Loads (Step d s) l u) =
        let nodeLabel = show $ Place start d
        in  printf "%s -> %s [label=\"@%d:+%d,-%d\"];" nodeLabel nodeLabel s l u

fromUnload :: [Int] -> Loads
fromUnload (g : q : s : d : _) = Loads (Step d s) q 0

fromLoad :: [Int] -> Loads
fromLoad (g : q : s : d : _) = Loads (Step d s) 0 q

-- Returns a map from truck to good to load actions
strictLoadSchedules
    :: String -> ([Int] -> Loads) -> FactProgram -> Map.Map Int (SetMap Loads)
strictLoadSchedules k f program =
    foldr update Map.empty $ facts program Map.! k
    where update (tr : g : q : vals) = nestedInsert tr g $ f (g : q : vals)

loadSchedules :: FactProgram -> Map.Map Int [Loads]
loadSchedules p =
    let mapper = Map.map (foldr mergeLoads [] . Set.toAscList . toUnion)
        ls     = mapper (strictLoadSchedules "load" fromLoad p)
        us     = mapper (strictLoadSchedules "unload" fromUnload p)
    in  Map.unionWith zipLoads ls us
  where
    toUnion :: Ord a => SetMap a -> Set.Set a
    toUnion = Map.foldr Set.union Set.empty
    mergeLoads :: Loads -> [Loads] -> [Loads]
    mergeLoads l1 [] = [l1]
    mergeLoads l1 (l2 : tl) | lstep l1 < lstep l2 = l1 : l2 : tl
                            | otherwise           = (l1 <> l2) : tl
    zipLoads :: [Loads] -> [Loads] -> [Loads]
    zipLoads (l : ls) = (l :) . zipWith (<>)
                                         -- prepend neutral load because head
                                         -- was removed
                                        (ls ++ [Loads (Step 0 0) 0 0])

initPos :: [Int] -> Bool
initPos (_ : _ : 1 : 1 : _) = True
initPos _                   = False

truckStarts :: FactProgram -> Map.Map Int Int
truckStarts program =
    Map.fromList
        .     Set.toList
        .     Set.map twoHeads
        .     Set.filter initPos
        $     facts program
        Map.! "truck_at"
    where twoHeads (t : p : _) = (t, p)
