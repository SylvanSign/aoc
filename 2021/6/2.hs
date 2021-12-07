import qualified Data.Map as Map
import Debug.Trace (trace)

main = do
  input <- parseInput
  print $ simulation input

simulation :: Map.Map Int Int -> Int
simulation fishes = (sum . Map.elems . foldr simulate fishes) [1 .. 256]
  where
    simulate time updatedMap = Map.foldrWithKey countdown Map.empty updatedMap
      where
        countdown fishDay fishCount fishMap =
          let updatedMap = case fishDay - 1 of
                -1 ->
                  (Map.insertWith (+) 6 fishCount . Map.insert 8 fishCount) fishMap
                nextDay ->
                  Map.insert nextDay fishCount fishMap
           in updatedMap

parseInput :: IO (Map.Map Int Int)
parseInput = do
  input <- readFile "input"
  let nums = (map read . splitOn ',') input
      fishMap = foldr (\fish -> Map.insertWith (+) fish 1) Map.empty nums
  return fishMap

splitOn :: Char -> String -> [String]
splitOn char s = case dropWhile (== char) s of
  "" -> []
  s' -> w : splitOn char s''
    where
      (w, s'') = break (== char) s'
