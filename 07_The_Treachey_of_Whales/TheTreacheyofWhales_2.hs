import Data.List.Split
import Data.Function

main = interact $ show . minDist . parse
  where parse   = map read . splitOn "," . head . lines
        minDist = sum . (map =<< dist . avg)
        dist    = ((.) (scanl (+) 0 [1..] !!) abs .) . (-)
        avg     = div <$> sum <*> length
