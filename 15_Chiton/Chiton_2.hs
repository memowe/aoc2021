import Data.Char
import Data.Maybe
import qualified Data.Map as M
import Algorithm.Search

type Coord  = (Int, Int)
type Risk   = M.Map Coord Int

maxX, maxY :: Risk -> Int
maxX risks = maximum $ map fst $ M.keys risks
maxY risks = maximum $ map snd $ M.keys risks

legal :: Risk -> Coord -> Bool
legal risks (x,y) = x >= 0 && y >= 0 && x <= maxX risks && y <= maxY risks

manDist :: Coord -> Coord -> Int
manDist (x1,y1) (x2,y2) = abs (x2 - x1) + abs (y2 - y1)

edge :: Risk -> Coord -> Coord -> Maybe Int
edge risks c1 c2  | manDist c1 c2 == 1  = c2 `M.lookup` risks
                  | otherwise           = Nothing

expand :: Int -> Risk -> Risk
expand n risks = M.fromList . expand' . M.toList $ risks
  where w = maxX risks + 1; h = maxY risks + 1
        ((x,y), r) `with` (i,j) = ((x + w*i, y + h*j), ((r-1+i+j) `mod` 9)+1)
        expand' risks = do  xs <- [0 .. n-1]; ys <- [0 .. n-1]
                            rs <- risks
                            return $ rs `with` (xs,ys)

pathCost :: Risk -> Int
pathCost risks = fst $ fromJust $ dijkstra neighbors edgeCost done (0,0)
  where neighbors (x,y) = filter (legal risks) [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
        edgeCost a b    = fromJust $ edge risks a b
        done coord      = coord == (maxX risks, maxY risks)

parse :: String -> Risk
parse = M.fromList . coordRisks . map (digitToInt <$>) . lines
  where coordRisks ints = do  x <- [0 .. pred $ length ints]
                              y <- [0 .. pred $ length $ head ints]
                              return ((x,y), ints !! y !! x)

main = interact $ show . pathCost . expand 5 . parse
