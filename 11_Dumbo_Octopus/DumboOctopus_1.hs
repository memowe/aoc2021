import Data.Char
import Data.Maybe
import Data.List
import Control.Monad

import Debug.Trace

range :: [a] -> [Int]
range = enumFromTo 0 . pred . length

type Octopus  = Maybe Int
type Octopii  = [[Octopus]]
type Position = (Int, Int)

showOctopii :: Octopii -> String
showOctopii = unlines . map (so <$>)
  where so (Just i) = head $ show i
        so Nothing  = '_'

step :: Octopus -> Octopus
step old = do new <- (+1) <$> old
              guard $ new <= 9
              return new

hasFlashed :: Octopus -> Bool
hasFlashed = isNothing

octopus :: Octopii -> Position -> Maybe Octopus
octopus os (x,y) = do guard $ y `elem` range os
                      let row = os !! y
                      guard $ x `elem` range row
                      return $ row !! x

setOctopus :: Octopii -> Position -> Octopus -> Octopii
setOctopus os (x,y) o = let (ar,row:br) = splitAt y os
                            (ac, _ :bc) = splitAt x row
                            newRow      = ac ++ [o] ++ bc
                        in  ar ++ [newRow] ++ br

positions :: Octopii -> [Position]
positions os = do x <- range os
                  y <- range $ head os
                  return (x,y)

neighborhood :: Position -> [Position]
neighborhood (x,y) = do xs <- spread <*> return x
                        ys <- spread <*> return y
                        return (xs,ys)
                      where spread = [pred, id, succ]

octopiiStep :: Octopii -> Octopii
octopiiStep os  = let os'     = map (step <$>) os
                      pos     = filter (isJust . octopus os') (positions os')
                      flaPos  = filter (hasFlashed . fromJust . octopus os') pos
                      neiPos  = nub $ concatMap neighborhood flaPos
                  in  processOctopiiStep neiPos os'

processOctopiiStep :: [Position] -> Octopii -> Octopii
processOctopiiStep ps os | trace (show ps ++ "\n" ++ showOctopii os) False = undefined
processOctopiiStep []     os = os
processOctopiiStep (p:ps) os =
  case octopus os p of
    Nothing -> processOctopiiStep ps os
    Just o  -> let  os' = setOctopus os p (step o)
                    ps' = nub $ ps ++ neighborhood p
                in  processOctopiiStep ps' os'

valid :: Octopii -> Position -> Bool
valid = ((isJust . join) .) . octopus

readOctopii :: String -> Octopii
readOctopii = map (return . digitToInt <$>) . lines
