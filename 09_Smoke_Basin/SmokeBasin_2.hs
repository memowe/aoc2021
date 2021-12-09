import Data.Char
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

neighborhood :: Enum a => Location -> Height a -> [Location]
neighborhood (x,y) hs = do  x' <- range x
                            y' <- range y
                            guard $ isLegal (x',y') hs
                            return (x',y')
  where range = enumFromTo <$> pred  <*> succ

main = interact $ show . parse
  where parse = map (digitToInt <$>) . lines
