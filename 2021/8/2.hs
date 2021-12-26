import Data.List
import Data.Ord (Down (Down))
import Data.Semigroup (diff)

main = do
  pairs <- pairs "input"
  print $ foldr uniqueFolder 0 pairs

solve :: [String] -> String
solve inputs = foo
  where
    foo = "todo"

inputs = ["acedgfb", "cdfbe", "gcdfa", "fbcad", "dab", "cefabd", "cdfgeb", "eafb", "cagedb", "ab"]

two = (head . lengthsOf 2) inputs

three = (head . lengthsOf 3) inputs

four = (head . lengthsOf 4) inputs

fives = lengthsOf 5 inputs

sixes = lengthsOf 6 inputs

seven = (head . lengthsOf 7) inputs

a = three \\ two

b = intersectFold (four : sixes) \\ two

c = two \\ f

d = differenceFold $ four : [b, c, f]

e = differenceFold [seven, four, three] \\ g

f = intersectFold (four : sixes) `intersect` two

g = intersectFold $ differenceFold [seven, four, three] : sixes

intersectFold :: [String] -> String
intersectFold = foldr1 intersect . sort

differenceFold :: [String] -> String
differenceFold = foldl1' (\\) . sortOn (Data.Ord.Down . length)

lengthsOf :: Int -> [String] -> [String]
lengthsOf len = filter ((== len) . length)

uniqueFolder :: ([String], [String]) -> Int -> Int
uniqueFolder (i, o) acc = acc + count
  where
    letterCounts = map length o
    uniques = filter (`elem` [2, 3, 4, 7]) letterCounts
    count = length uniques

pairs :: FilePath -> IO [([String], [String])]
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
