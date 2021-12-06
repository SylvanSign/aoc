import Data.List (sort)
import qualified Data.Map as Map

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
    points = pointsBetween p1 p2
    updatedMap = foldr (\p -> Map.insertWith (+) p 1) map points

pointsBetween :: Point -> Point -> [Point]
pointsBetween p1 p2
  | p1 == p2 = [p1]
pointsBetween p1@(Point x1 y1) p2@(Point x2 y2) = gatherPoints xMov yMov p1 p2
  where
    mover x y = case compare x y of
      LT -> succ
      GT -> pred
      EQ -> id
    xMov = mover x1 x2
    yMov = mover y1 y2

gatherPoints :: (Int -> Int) -> (Int -> Int) -> Point -> Point -> [Point]
gatherPoints xMov yMov p1@(Point x y) p2
  | p1 == p2 = [p2]
  | otherwise = p1 : gatherPoints xMov yMov (Point (xMov x) (yMov y)) p2

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
