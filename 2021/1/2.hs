main :: IO ()
main = do
  input <- readFile "input"
  let depths = map read $ lines input
      windowedDepths = zipWith3 sum3 depths (drop 1 depths) (drop 2 depths)
      (_, answer) = foldl myFunc (-1, -1) windowedDepths
  print answer

sum3 :: Num a => a -> a -> a -> a
sum3 a b c = a + b + c

myFunc :: (Ord a, Num b) => (a, b) -> a -> (a, b)
myFunc (prevDepth, count) curDepth
  | curDepth > prevDepth = (curDepth, count + 1)
  | otherwise = (curDepth, count)
