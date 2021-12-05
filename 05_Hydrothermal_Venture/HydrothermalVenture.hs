{-# LANGUAGE TupleSections #-}

import Data.List.Split
import qualified Data.Map as M
import Data.Function

type Coord  = (Int, Int)
type Line   = (Coord, Coord)

lineDx, lineDy :: Line -> Int
lineDx = abs . uncurry ((-) `on` fst)
lineDy = abs . uncurry ((-) `on` snd)

isHorizontal, isVertical :: Line -> Bool
isHorizontal  = (== 0) . lineDy
isVertical    = (== 0) . lineDx

horiCoords, vertiCoords :: Line -> [Coord]
horiCoords  ((x1,y),(x2,_))   = (,y) <$> range x1 x2
vertiCoords ((x,y1),(_,y2))   = (x,) <$> range y1 y2

coords :: Line -> [Coord]
coords line | isHorizontal  line  = horiCoords  line
            | isVertical    line  = vertiCoords line
            | otherwise           = error "Invalid line!"

range :: (Enum a, Ord a) => a -> a -> [a]
range a b = if a <= b then [a..b] else reverse [b..a]

countCoords :: [Line] -> M.Map Coord Int
countCoords = foldr add M.empty . concatMap coords
  where add = flip (M.insertWith (+)) 1

findMultiCoords :: [Line] -> [Coord]
findMultiCoords = M.keys . M.filter (> 1) . countCoords

parse :: String -> [Line]
parse = map (p " -> " $ p "," read) . lines
  where p s r = ((,) <$> head <*> head . tail) . map r . splitOn s

main = interact $ show . length . findMultiCoords . onlyRect . parse
  where onlyRect = filter $ (||) <$> isHorizontal <*> isVertical
