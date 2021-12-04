module GiantSquid.Parse where

import GiantSquid.Board
import Data.List.Split

parseInputBoards :: String -> ([Int], [Board Int])
parseInputBoards = ((,) <$> toInput <*> toBoards) . splitWhen null . lines
  where toInput   = map read . splitOn "," . head . head
        toBoards  = map (createBoard . map ((read <$>) . words)) . tail
