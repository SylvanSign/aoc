main :: IO ()
main = do
  input <- readFile "input"
  let inputLines = lines input
      (horizontal, depth, _) = foldl tracker (0, 0, 0) inputLines
      answer = horizontal * depth
  print answer

type SubPosition = (Int, Int, Int)

tracker :: SubPosition -> String -> SubPosition
tracker position@(_, _, aim) line =
  move position $ calculateDelta line aim

move :: SubPosition -> SubPosition -> SubPosition
move (h1, d1, a1) (h2, d2, a2) = (h1 + h2, d1 + d2, a1 + a2)

calculateDelta :: [Char] -> Int -> SubPosition
calculateDelta line aim =
  case command of
    "down" -> (0, 0, distance)
    "up" -> (0, 0, - distance)
    "forward" -> (distance, aim * distance, 0)
    _ -> error $ "parsed an unknown command " ++ command
  where
    [command, distanceStr] = words line
    distance = read distanceStr
