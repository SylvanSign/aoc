main = do
  pairs <- pairs "input"
  print $ foldr uniqueFolder 0 pairs

uniqueFolder :: ([String], [String]) -> Int -> Int
uniqueFolder (i, o) acc = acc + count
  where
    letterCounts = map length o
    uniques = filter (`elem` [2, 3, 4, 7]) letterCounts
    count = length uniques

pairs file = do
  text <- readFile file
  let pairs = (map ((\[i, o] -> (i, o)) . map words . splitOn '|') . lines) text
  return pairs

splitOn :: Char -> String -> [String]
splitOn char s = case dropWhile (== char) s of
  "" -> []
  s' -> w : splitOn char s''
    where
      (w, s'') = break (== char) s'
