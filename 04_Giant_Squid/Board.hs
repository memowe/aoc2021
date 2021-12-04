module Board where

import Data.Either
import Data.List
import Data.List.Split
import Data.Function

newtype Position a = Position { value :: Either a a }
newtype Board a    = Board    { rows :: [[Position a]] }

createBoard :: [[a]] -> Board a
createBoard = Board . map (Position . Left <$>)

hasWon :: Board a -> Bool
hasWon = ((||) `on` any rights) <$> rows <*> transpose . rows
  where rights = all (isRight . value)

select :: Eq a => a -> Board a -> Board a
select val = Board . map (spos <$>) . rows
  where spos p@(Position (Left v))
          | v == val  = Position (Right v)
          | otherwise = p
        spos p = p

parse :: String -> ([Int], [Board Int])
parse = ((,) <$> toInput <*> toBoards) . splitWhen null . lines
  where toInput   = map read . splitOn "," . head . head
        toBoards  = map (createBoard . map ((read <$>) . words)) . tail
