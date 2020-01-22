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

globals :: [String]
globals = ["edge [colorscheme=\"set19\"];"]

toGraph :: FactProgram -> [String]
toGraph p =
    let factories  = map head $ Set.toList $ facts p Map.! "factory"
        warehouses = map head $ Set.toList $ facts p Map.! "warehouse"
        drives     = driveSchedules p
        starts     = truckStarts p
        days       = consts p Map.! "days"
        dayPlacesGraph :: Int -> [String]
        dayPlacesGraph d =
                let fs = map (`Place` d) factories
                    ws = map (`Place` d) warehouses
                in  placesGraph fs ws
    in  ["digraph TruckSchedule {"]
            ++ globals
            ++ concatMap dayPlacesGraph [1 .. days + 1]
            ++ Map.foldr
                   (++)
                   []
                   (Map.mapWithKey
                       (\k -> toEdgeRec (Place (starts Map.! k) 1))
                       drives
                   )
            ++ ["}"]
  where
    toEdgeRec :: Place -> [Drives] -> [String]
    toEdgeRec _ []         = []
    toEdgeRec s (d : tail) = toEdge s d : toEdgeRec (endPlace d) tail

data Place = Place { pid :: Int
                   , pday :: Int } deriving (Eq, Ord)
instance Show Place where
    show (Place i d) = printf "n%d_%d" i d

type Factory = Place
type Warehouse = Place

placesGraph :: [Factory] -> [Warehouse] -> [String]
placesGraph fs ws =
    let day = pday $ head fs
    in  [ printf "subgraph cluster_%d {" day
        , printf "label=\"Day %d\";"     day
        , "color=black;"
        ]
            ++ map (shapedNode factoryShape)   fs
            ++ map (shapedNode warehouseShape) ws
            ++ ["}"]
  where
    factoryShape   = "triangle"
    warehouseShape = "house"
    shapedNode :: String -> Place -> String
    shapedNode shape p =
        printf "%s [shape=\"%s\",label=\"%d\"];" (show p) shape (pid p)

data Step = Step { day :: Int
                 , num :: Int } deriving (Eq, Ord)

data Drives = Drives { dstep :: Step
                     , to :: Int
                     , truck :: Int } deriving (Eq, Ord)

toEdge :: Place -> Drives -> String
toEdge from (Drives (Step d s) t tr) =
    let startNode = show from
        endNode   = show $ Place t d
    in  printf "%s -> %s [label=\"@%d\",color=%d];" startNode endNode s tr
endPlace :: Drives -> Place
endPlace (Drives (Step d _) t _) = (Place t d)

fromList :: [Int] -> Drives
fromList (truck : to : step : day : _) = Drives (Step day step) to truck

driveSchedules :: FactProgram -> Map.Map Int [Drives]
driveSchedules program =
    Map.map Set.toAscList . foldr update Map.empty $ facts program Map.! "drive"
    where update (tr : vals) = Map.alter (insertMaybe $ fromList (tr : vals)) tr

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
