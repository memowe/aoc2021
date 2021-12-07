import Data.List.Split

main = interact $ show . minDist . parse
  where parse   = map read . splitOn "," . head . lines
        minDist = sum . (map =<< dist . avg)
        dist    = (((`div` 2) . ((*) <*> succ) . abs) .) . (-)
        avg     = div <$> sum <*> length
