module Dive_2 where

data Mov    = Trn | Fwd
data Cmd    = Cmd Mov Int
type State  = (Int, Int, Int)

parse :: String -> Cmd
parse s = let [dir,a] = words s; amount = read a
          in  case dir of "forward" -> Cmd Fwd amount
                          "up"      -> Cmd Trn (- amount)
                          "down"    -> Cmd Trn amount

goto :: State -> Cmd -> State
goto (x,y,aim) (Cmd Fwd fwd) = (x + fwd, y + aim * fwd, aim)
goto (x,y,aim) (Cmd Trn trn) = (x, y, aim + trn)

main = interact (show . prod . dive . map parse . lines)
  where dive          = foldl goto (0,0,0)
        prod (x,y,_)  = x * y
