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
        starts = truckStarts p
    in  Map.foldr (++) [] $ Map.mapWithKey (\k -> toEdgeRec 0) drives
  where
    toEdgeRec :: Int -> [Drives] -> [String]
    toEdgeRec _ []         = []
    toEdgeRec s (d : tail) = toEdge s d : toEdgeRec (num $ dstep d) tail

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
