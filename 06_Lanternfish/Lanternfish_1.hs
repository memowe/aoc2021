import Data.Bool
import Data.List.Split
import Data.Tree

main = interact $ show . length . (!! 80) . gens . create . parse
  where parse   = map read . splitOn "," . head . lines
        create  = Node <$> head <*> map return . tail
        gens    = iterate (>>= tick)
        tick    = bool (Node 6 [return 8]) <$> return . pred <*> (> 0)
