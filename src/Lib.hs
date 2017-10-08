{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS -Wall #-}

module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Distance = Double
data Point = Point Integer Integer deriving (Show, Eq)
data Segment = Segment Point Point deriving (Show, Eq)
type Path = [Segment]
type PathList = [Path]

newtype ByDistance = ByDistance { unByDistance :: Path } deriving (Eq)
newtype ByStops = ByStops { unByStops :: Path } deriving (Eq)

instance Ord ByDistance where
  (ByDistance x) `compare` (ByDistance y) = pathDistance x `compare` pathDistance y


instance Ord ByStops where
  (ByStops x) `compare` (ByStops y) = pathStops x `compare` pathStops y

segmentDistance :: Segment -> Distance
segmentDistance (Segment (Point x y) (Point x' y')) =
  sqrt . fromIntegral $ ((x' - x)^2 + (y' - y)^2)

pathDistance :: Path -> Distance
-- pathDistance = getSum . foldMap (Sum . segmentDistance)
pathDistance = sum . fmap segmentDistance

pathStops :: Path -> Int
pathStops = length

-- isShorterPath :: Path -> Path -> Bool
-- isShorterPath x y = pathDistance x < pathDistance y

shortestPath :: PathList -> Path
--shortestPath = foldl (\acc x -> if isShorterPath acc x then acc else x) emptyPath
shortestPath = unByDistance . minimum . fmap ByDistance

leastStopsPath :: PathList -> Path
--leastStopsPath = foldl (\acc x -> if (pathStops x) < (pathStops acc) then x else acc) (replicate 10 (Segment (Point 1 1) (Point 2 2)))
leastStopsPath = unByStops . minimum . fmap ByStops

-- Finding the minimum (shortest path or path with the least stop) is not a monoidal operation, it's only a semigroup as we can't create a zero value, there we could use minimum as above but it will fail miserably when trying to find the minimum of an empty list

enumeratePathStops :: Path -> [Point]
enumeratePathStops path = path >>= (\(Segment p p') -> [p, p'])

contains :: Point -> Path -> Bool
contains x path = x `elem` enumeratePathStops path

containsAll :: [Point] -> Path -> Bool
containsAll xs path = all (\x -> contains x path) xs

pathsStoppingAt :: [Point] -> PathList -> PathList
pathsStoppingAt stops = filter (containsAll stops)

shortestPathStoppingAt :: Point -> PathList -> Path
shortestPathStoppingAt x = shortestPath . pathsStoppingAt [x]

