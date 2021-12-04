import GiantSquid.Board
import GiantSquid.Parse
import Data.Either
import Data.Maybe
import Data.List

firstWinningScore :: (Eq a, Num a) => [a] -> [Board a] -> a
firstWinningScore input boards =
  let moves       = scanl draw (boards, 0) input
      (bs, last)  = fromJust $ find (any hasWon . fst) moves
  in  last * sum (unmarked $ fromJust $ find hasWon bs)
  where draw (bs,_) i = (map (select i) bs, i)
        unmarked      = concatMap ((fromLeft 0 . value) <$>) . rows

main :: IO ()
main = interact $ show . uncurry firstWinningScore . parseInputBoards
