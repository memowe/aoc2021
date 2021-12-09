import Data.Char
import Data.List
import Data.Maybe
import Control.Monad

type Height a = [[a]]
type Location = (Int, Int)

locations :: Height a -> [Location]
locations hs = do x <- range (head hs)
                  y <- range hs
                  return (x,y)
  where range = enumFromTo 0 . pred . length

isLegal :: Location -> Height a -> Bool
isLegal (x,y) hs =    y >= 0 && y < length hs
                  &&  x >= 0 && x < length (head hs)

height :: Location -> Height a -> Maybe a
height (x,y) hs = do  guard $ isLegal (x,y) hs
                      return $ hs !! y !! x

neighborhood :: Location -> Height a -> [Location]
neighborhood (x,y) hs = do  x' <- range x
                            y' <- range y
                            guard $ isLegal (x',y') hs
                            return (x',y')
  where range = enumFromTo <$> pred  <*> succ

isLowPoint :: Ord a => Location -> Height a -> Bool
isLowPoint loc hs = let nbs = neighborhood loc hs \\ [loc]
                        hts = fromJust . (`height` hs) <$> nbs
                    in  all (> (fromJust $ height loc hs)) hts

riskLevel :: (Num a, Ord a) => Location -> Height a -> Maybe a
riskLevel loc hs = do risk <- height loc hs
                      guard $ loc `isLowPoint` hs
                      return $ risk + 1

risk :: Height Int -> Int
risk  = sum . (mapMaybe <$> flip riskLevel <*> locations)

main = interact $ show . risk . parse
  where parse = map (digitToInt <$>) . lines
