import Data.Tuple
import Data.List
import Data.Map ( Map, fromList, toList, (!), member, keys, elems )
import Data.Function

brackets, cloBra :: Map Char Char
brackets = fromList . map (((,) `on` head) <*> tail) . words $ "() [] {} <>"
cloBra = fromList . map swap . toList $ brackets

data Result = Legal | Incomplete String | Illegal Char

collect :: String -> String -> Result
collect ""      ""                                = Legal
collect s       ""                                = Incomplete $ rev s
collect s       (c:cs)  |     c `member` brackets = collect (c:s) cs
collect (s:ss)  (c:cs)  |     s `member` brackets
                          &&  c `member` cloBra
                          &&  c == brackets ! s   = collect ss cs
collect _       (c:_)                             = Illegal c

rev :: String -> String
rev = map (brackets !)

scoreIncomplete :: [Result] -> [Int]
scoreIncomplete = map (score . fromInc) . filter isInc
  where isInc   (Incomplete _) = True; isInc _ = False
        fromInc (Incomplete s) = s

score :: String -> Int
score = foldl ((. bs) . (+) . (*5)) 0
  where bs ')' = 1; bs ']' = 2; bs '}' = 3; bs '>' = 4; bs _ = 42

autoComplete :: [String] -> Int
autoComplete = middle . scoreIncomplete . map (collect "")
  where middle = (!!) <$> sort <*> (`div` 2) .length

main = interact $ show . autoComplete . lines
