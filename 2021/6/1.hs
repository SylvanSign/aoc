main = do
  input <- parseInput
  print $ simulation input

simulation :: [Int] -> Int
simulation fishes = (length . foldr simulate fishes) [1 .. 80]
  where
    simulate _ = foldr countdown []
      where
        countdown fish updatedFishes =
          case fish - 1 of
            -1 -> 6 : 8 : updatedFishes
            other -> other : updatedFishes

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
