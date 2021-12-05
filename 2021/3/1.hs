import Data.Char (digitToInt)

type BitList = [Int]

main :: IO ()
main = do
  input <- readFile "input"
  let inputLines = bitStringToBitList $ lines input
      starter = [0 | _ <- head inputLines]
      tally = foldl tallyMostCommonBits starter inputLines
      gamma = tallyToGammaRate tally
      epsilon = flipBits gamma
      gammaDecimal = binaryToDecimal gamma
      epsilonDecimal = binaryToDecimal epsilon
  mapM_ print [gamma, epsilon]
  mapM_ print [gammaDecimal, epsilonDecimal]
  print $ gammaDecimal * epsilonDecimal

binaryToDecimal :: BitList -> Int
binaryToDecimal bits = decimal
  where
    (_, decimal) = foldr decimalFolder (0, 0) bits
      where
        decimalFolder cur (twos, acc) = (twos + 1, acc + cur * 2 ^ twos)

flipBits :: BitList -> BitList
flipBits = map (\x -> if x == 0 then 1 else 0)

tallyToGammaRate :: BitList -> BitList
tallyToGammaRate = map (\x -> if x > 0 then 1 else 0)

bitStringToBitList :: [String] -> [BitList]
bitStringToBitList = map $ map digitToInt

tallyMostCommonBits :: BitList -> BitList -> BitList
tallyMostCommonBits = zipWith tally
  where
    tally tallyBit lineBit = tallyBit + modifier
      where
        modifier = if lineBit == 0 then (-1) else 1
