import Data.List.Split

main = interact $ show . minDist . parse
  where parse   = map read . splitOn "," . head . lines
        minDist = minimum . ((<$>) <$> (sum .) . flip (map . dist) <*> range)
        dist    = ((.) (scanl (+) 0 [1..] !!) abs .) . (-)
        range   = enumFromTo <$> minimum <*> maximum
