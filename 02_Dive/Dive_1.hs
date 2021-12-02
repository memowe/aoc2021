parse :: String -> (Int, Int)
parse s = let [dir,a] = words s; amount = read a
          in  case dir of "forward" -> (amount, 0)
                          "up"      -> (0, - amount)
                          "down"    -> (0, amount)

main = interact (show . prod . dive . map parse . lines)
  where dive                = foldl goto (0,0)
        prod (x,y)          = x * y
        goto (x,y) (px,py)  = (px + x, py + y)
