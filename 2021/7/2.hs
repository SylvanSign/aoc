import Data.List
import Debug.Trace (trace)

main = do
  input <- parseInput
  print $ manyCrabs input

manyCrabs :: [Int] -> Int
manyCrabs positions = minimum $ map (`crabs` positions) [floor avg]
  where
    avg = fromIntegral (sum positions) / fromIntegral (length positions)

crabs :: Int -> [Int] -> Int
crabs spot = foldr (\p acc -> acc + fuelCost p spot) 0
  where
    fuelCost x y =
      let n = abs (x - y)
       in n * (n + 1) `div` 2

parseInput :: IO [Int]
parseInput = do
  input <- readFile "sample"
  return $ (map read . splitOn ',') input

splitOn :: Char -> String -> [String]
splitOn char s = case dropWhile (== char) s of
  "" -> []
  s' -> w : splitOn char s''
    where
      (w, s'') = break (== char) s'
