main :: IO ()
main = do
  input <- readFile "input"
  let inputLines = lines input
      (horizontal, depth) = foldl tracker (0, 0) inputLines
      answer = horizontal * depth
  print answer

type SubPosition = (Int, Int)

tracker :: SubPosition -> String -> SubPosition
tracker position line =
  move position $ calculateDelta line

move :: SubPosition -> SubPosition -> SubPosition
move (h1, d1) (h2, d2) = (h1 + h2, d1 + d2)

calculateDelta :: [Char] -> SubPosition
calculateDelta line =
  case command of
    "forward" -> (distance, 0)
    "down" -> (0, distance)
    "up" -> (0, - distance)
    _ -> error $ "parsed an unknown command " ++ command
  where
    [command, distanceStr] = words line
    distance = read distanceStr
