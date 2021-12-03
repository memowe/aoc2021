import Data.List
import Data.Function

lifeSupport :: [[Bool]] -> Int
lifeSupport     = mult <$> select 0 oxy <*> select 0 co2
  where mult    = (*) `on` toInt
        toInt   = foldl ((. fromEnum) . (+) . (*2)) 0
        oxy     = elemIndices =<< (>=0) . bitSum
        co2     = elemIndices =<< (< 0) . bitSum
        bitSum  = sum . map (pred . (*2) . fromEnum)

select :: Int -> ([Bool] -> [Int]) -> [[Bool]] -> [Bool]
select n crit lines
  | n >= length (head lines)  = []
  | length selected == 1      = head selected
  | otherwise                 = select (n+1) crit selected
  where selected = map (lines !!) (crit (map (!! n) lines))

main = interact $ show . lifeSupport . prepare
  where prepare = map (map (== '1')) . lines
