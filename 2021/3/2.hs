import Data.Char (digitToInt)

type Bit = Int

type BitList = [Bit]

main :: IO ()
main = do
  input <- readFile "input"
  let inputLines = bitStringToBitList $ lines input
      oxygen = oxygenRating inputLines
      co2 = co2Rating inputLines
      oxygenDecimal = binaryToDecimal oxygen
      co2Decimal = binaryToDecimal co2
  mapM_ print [oxygen, co2]
  mapM_ print [oxygenDecimal, co2Decimal]
  print $ oxygenDecimal * co2Decimal

sample :: IO [BitList]
sample = do
  input <- readFile "sample"
  return $ bitStringToBitList $ lines input

binaryToDecimal :: BitList -> Int
binaryToDecimal bits = decimal
  where
    (_, decimal) = foldr decimalFolder (0, 0) bits
      where
        decimalFolder cur (twos, acc) = (twos + 1, acc + cur * 2 ^ twos)

bitStringToBitList :: [String] -> [BitList]
bitStringToBitList = map $ map digitToInt

rating :: (Int -> [BitList] -> Bit) -> [BitList] -> BitList
rating bitFilter lines =
  let len = length . head $ lines
   in head $ superFilter bitFilter 0 len lines

oxygenRating :: [BitList] -> BitList
oxygenRating = rating mostCommonBit

co2Rating :: [BitList] -> BitList
co2Rating = rating leastCommonBit

superFilter :: (Int -> [BitList] -> Bit) -> Int -> Int -> [BitList] -> [BitList]
superFilter bitFilter pos len lines
  | pos == len = lines
  | length lines == 1 = lines
  | otherwise =
    let filteredLines = myFilter bitFilter pos lines
     in superFilter bitFilter (pos + 1) len filteredLines

myFilter :: (Int -> [BitList] -> Bit) -> Int -> [BitList] -> [BitList]
myFilter bitFilter pos lines = filterByBit (bitFilter pos lines) pos lines

filterByBit :: Bit -> Int -> [BitList] -> [BitList]
filterByBit bit pos = filter pred
  where
    pred = (== bit) . (!! pos)

mostCommonBit :: Int -> [BitList] -> Bit
mostCommonBit pos lines =
  if tallyMostCommonBit pos lines >= 0
    then 1
    else 0

leastCommonBit :: Int -> [BitList] -> Bit
leastCommonBit pos lines =
  abs $ mostCommon - 1 -- flip the bit
  where
    mostCommon = mostCommonBit pos lines

tallyMostCommonBit :: Int -> [BitList] -> Bit
tallyMostCommonBit pos = foldl tally 0
  where
    tally acc lines = acc + modifier
      where
        lineBit = lines !! pos
        modifier = if lineBit == 0 then (-1) else 1
