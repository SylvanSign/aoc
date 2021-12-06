import Data.List (sort)
import qualified Data.Map as Map
import Debug.Trace (trace)

data Point = Point Int Int deriving (Eq, Ord, Show)

data Line = Line Point Point deriving (Eq, Ord, Show)

main = do
  lines <- parseInput
  let includingDiagonals = lines
      pointCounts = foldr pointCointer Map.empty includingDiagonals
      dangerousSpotCount = (length . filter (> 1)) $ Map.elems pointCounts
  print dangerousSpotCount

pointCointer :: Line -> Map.Map Point Int -> Map.Map Point Int
pointCointer line@(Line p1@(Point x1 y1) p2@(Point x2 y2)) map = updatedMap
  where
    [minX, maxX] = sort [x1, x2]
    [minY, maxY] = sort [y1, y2]
    points =
      sort
        [ p3
          | x <- [minX .. maxX],
            y <- [minY .. maxY],
            let p3 = Point x y,
            between p1 p2 p3
        ]
    updatedMap = foldr (\p -> Map.insertWith (+) p 1) map points

between :: Point -> Point -> Point -> Bool
between x y z = abs (crossProduct x y z) == 0 && dotProduct x y z <= squaredLength x y

crossProduct :: Point -> Point -> Point -> Int
crossProduct (Point x1 y1) (Point x2 y2) (Point x3 y3) =
  (y3 - y1) * (x2 - x1) - (x3 - x1) * (y2 - y1)

dotProduct :: Point -> Point -> Point -> Int
dotProduct (Point x1 y1) (Point x2 y2) (Point x3 y3) =
  (x3 - x1) * (x2 - x1) + (y3 - y1) * (y2 - y1)

squaredLength :: Point -> Point -> Int
squaredLength (Point x1 y1) (Point x2 y2) =
  (x2 - x1) ^ 2 + (y2 - y1) ^ 2

nonDiagonal :: Line -> Bool
nonDiagonal (Line (Point x1 y1) (Point x2 y2)) = x1 == x2 || y1 == y2

parseInput :: IO [Line]
parseInput = do
  input <- readFile "input"
  let parsedCoordsStrings = map ((map (splitOn ',') . filter (/= "->")) . words) . lines $ input
      parsed = map parseLineFromCoords parsedCoordsStrings
  return parsed

parseLineFromCoords :: [[String]] -> Line
parseLineFromCoords [[x1, y1], [x2, y2]] =
  Line (Point (read x1) (read y1)) (Point (read x2) (read y2))
parseLineFromCoords _ = error "yikers"

splitOn :: Char -> String -> [String]
splitOn char s = case dropWhile (== char) s of
  "" -> []
  s' -> w : splitOn char s''
    where
      (w, s'') = break (== char) s'
