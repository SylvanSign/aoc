import Data.List
import Debug.Trace (trace)

type Board = [[Int]]

main = go "input"

go file = do
  input <- readFile file
  let (drawnNumbers, boards) = readInput input
      (_, winningScoresReversed) = foldl checkBoards (boards, []) drawnNumbers
      winningScores = reverse winningScoresReversed
  print winningScores
  print $ length winningScores
  print $ length boards

checkBoards :: ([Board], [Int]) -> Int -> ([Board], [Int])
checkBoards (boards, winningScores) num = (losers, updatedScores)
  where
    markedBoards = map (mark num) boards
    (winners, losers) = partition wonBoard markedBoards
    updatedScores = map ((* num) . scoreWinner) winners ++ winningScores

readInput :: String -> ([Int], [Board])
readInput input = (drawnNumbers, boards)
  where
    drawnNumberStrings : boardsStrings = filter (not . null) $ lines input
    drawnNumbers = map read $ splitOn ',' drawnNumberStrings
    boards = parseBoards boardsStrings

parseBoards :: [String] -> [Board]
parseBoards = chunks 5 . map (map read . words)

scoreWinner :: Board -> Int
scoreWinner = sum . filter (/= -1) . concat

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

splitOn :: Char -> String -> [String]
splitOn char s = case dropWhile (== char) s of
  "" -> []
  s' -> w : splitOn char s''
    where
      (w, s'') = break (== char) s'

chunks :: Int -> [a] -> [[a]]
chunks n = takeWhile (not . null) . unfoldr (Just . splitAt n)
