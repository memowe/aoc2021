import Data.List
import Data.List.Split

main = interact $ show . minDist . parse
  where parse   = map read . splitOn "," . head . lines
        minDist = sum . (dists . median <*> id)
        dists   = map . (abs .) . (-)
        median  = (!!) <$> sort <*> flip quot 2 . length
