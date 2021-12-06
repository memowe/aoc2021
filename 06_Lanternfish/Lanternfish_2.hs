{-# LANGUAGE TupleSections #-}

import Data.List.Split
import qualified Data.Map as M

main = interact $ show . sum . M.elems . (!! 256) . gens . count . parse
  where parse = map read . splitOn "," . head . lines
        count = M.fromListWith (+) . map (,1)
        gens  = iterate tick
        tick  = M.fromListWith (+) . concatMap tick' . M.toList
        tick' (days, count) | days == 0 = [(6, count), (8, count)]
                            | otherwise = [(days - 1, count)]
