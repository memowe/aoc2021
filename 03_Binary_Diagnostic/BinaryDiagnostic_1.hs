import Data.List
import Data.Function

main = interact $ show . combine . democracy . map toBools . lines
  where toBools   = map (== '1')
        democracy = map most . transpose
        most      = (>) <$> sum . map fromEnum <*> (`div` 2) . length
        combine   = ((*) `on` toInt) <*> map not
        toInt     = foldl ((. fromEnum) . (+) . (*2)) 0
