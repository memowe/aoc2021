import Data.List.Split
import Data.Function

main = interact $ show . countUnique . parse
  where parse       = concatMap parseLine . lines
        parseLine   = words . (!! 1) . splitOn " | "
        countUnique = length . filter ((`elem` [2,4,3,7]) . length)
