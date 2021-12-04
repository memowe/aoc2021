import Board
import Data.Either
import Data.Maybe
import Data.List

lastWinningScore :: (Eq a, Num a) => [a] -> [Board a] -> a
lastWinningScore input boards =
  let moves             = scanl draw (boards, 0) input
      (incomp, comp:_)  = break (all hasWon . fst) moves
      lastBoards        = fst $ last incomp
      lastBoard         = fromJust $ find (not . hasWon) lastBoards
      lastNum           = snd comp
      completeLastBoard = select lastNum lastBoard
  in lastNum * sum (unmarked completeLastBoard)
  where draw (bs,_) i = (map (select i) bs, i)
        unmarked      = concatMap ((fromLeft 0 . value) <$>) . rows

main :: IO ()
main = interact $ show . uncurry lastWinningScore . parse
