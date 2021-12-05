import Data.List

type Board = [[Int]]

main = do
  input <- readFile "input"
  let (drawnNumbers, boards) = readInput input
      winningScore =
        case foldl checkBoards (boards, Nothing) drawnNumbers of
          (_, Just winningScore) -> winningScore
          _ -> error "yikers"
  print winningScore

readInput :: String -> ([Int], [Board])
readInput input = (drawnNumbers, boards)
  where
    drawnNumberStrings : boardsStrings = filter (not . null) $ lines input
    drawnNumbers = map read $ splitOn ',' drawnNumberStrings
    boards = parseBoards boardsStrings

scoreWinner :: Board -> Int
scoreWinner = sum . filter (/= -1) . concat

checkBoards :: ([Board], Maybe Int) -> Int -> ([Board], Maybe Int)
checkBoards (boards, Just winningScore) num = (boards, Just winningScore)
checkBoards (boards, Nothing) num = (markedBoards, winningScore)
  where
    markedBoards = map (mark num) boards
    winningScore =
      case winner markedBoards of
        Just w -> Just $ scoreWinner w * num
        Nothing -> Nothing

wonBoard :: Board -> Bool
wonBoard board = rows || cols
  where
    winCondition = any $ all (== -1)
    rows = winCondition board
    cols = winCondition $ transpose board

winner :: [Board] -> Maybe Board
winner = find wonBoard

mark :: Int -> Board -> Board
mark num = map (map $ \x -> if x == num then -1 else x)

parseBoards :: [String] -> [Board]
parseBoards boardsStrings = chunks 5 $ map (map read . words) boardsStrings

splitOn :: Char -> String -> [String]
splitOn char s = case dropWhile (== char) s of
  "" -> []
  s' -> w : splitOn char s''
    where
      (w, s'') = break (== char) s'

chunks :: Int -> [a] -> [[a]]
chunks n = takeWhile (not . null) . unfoldr (Just . splitAt n)
