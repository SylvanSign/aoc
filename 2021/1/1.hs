main :: IO ()
main = do
  input <- readFile "input"
  let depths = map read $ lines input
      (_, answer) = foldl depthCounter (-1, -1) depths
  print answer

depthCounter :: (Ord a, Num b) => (a, b) -> a -> (a, b)
depthCounter (prevDepth, count) curDepth
  | curDepth > prevDepth = (curDepth, count + 1)
  | otherwise = (curDepth, count)
