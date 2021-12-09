import Data.List

main = do
  input <- parseInput
  print $ crabs input

crabs :: [Int] -> Int
crabs positions =
  let sorted = sort positions
      paired = take (length positions `div` 2) $ zip (reverse sorted) sorted
   in foldr (\(x, y) acc -> acc + x - y) 0 paired

parseInput :: IO [Int]
parseInput = do
  input <- readFile "input"
  return $ (map read . splitOn ',') input

splitOn :: Char -> String -> [String]
splitOn char s = case dropWhile (== char) s of
  "" -> []
  s' -> w : splitOn char s''
    where
      (w, s'') = break (== char) s'
