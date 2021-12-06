import Data.List (sort)
import qualified Data.Map as Map

data Point = Point Int Int deriving (Eq, Ord, Show)

data Line = Line Point Point deriving (Eq, Ord, Show)

main = do
  lines <- parseInput
  let nonDiagonals = filter nonDiagonal lines
      pointCounts = foldr pointCointer Map.empty nonDiagonals
      dangerousSpotCount = (length . filter (> 1)) $ Map.elems pointCounts
  print dangerousSpotCount

pointCointer :: Line -> Map.Map Point Int -> Map.Map Point Int
pointCointer (Line (Point x1 y1) (Point x2 y2)) map = updatedMap
  where
    [minX, maxX] = sort [x1, x2]
    [minY, maxY] = sort [y1, y2]
    points = [Point x y | x <- [minX .. maxX], y <- [minY .. maxY]]
    updatedMap = foldr (\p -> Map.insertWith (+) p 1) map points

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
