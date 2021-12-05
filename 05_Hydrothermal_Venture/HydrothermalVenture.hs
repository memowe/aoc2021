import Data.List.Split
import qualified Data.Map as M
import Data.Function

type Coord  = (Int, Int)
type Line   = (Coord, Coord)

isRect, isHorizontal, isVertical :: Line -> Bool
isRect        = (||) <$> isHorizontal <*> isVertical
isHorizontal  = uncurry ((==) `on` snd)
isVertical    = uncurry ((==) `on` fst)

rectCoords :: Line -> [Coord]
rectCoords ((x1,y1),(x2,y2)) = (,)  <$> [min x1 x2 .. max x1 x2]
                                    <*> [min y1 y2 .. max y1 y2]

countCoords :: [Line] -> M.Map Coord Int
countCoords = foldr add M.empty . concatMap rectCoords
  where add = flip (M.insertWith (+)) 1

findMultiCoords :: [Line] -> [Coord]
findMultiCoords = M.keys . M.filter (> 1) . countCoords

parse :: String -> [Line]
parse = map (p " -> " $ p "," read) . lines
  where p s r = ((,) <$> head <*> head . tail) . map r . splitOn s

main = interact $ show . length . findMultiCoords . filter isRect . parse
