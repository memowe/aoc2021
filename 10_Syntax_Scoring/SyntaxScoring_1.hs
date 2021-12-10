import Data.Map ( Map, fromList, (!), member, keys, elems )
import Data.Maybe
import Data.Function

brackets :: Map Char Char
brackets = fromList $ map (((,) `on` head) <*> tail) $ words "() [] {} <>"

collect :: String -> String -> Maybe Char
collect _ []                                          = Nothing
collect s       (c:cs)  |     c `elem` keys brackets  = collect (c:s) cs
collect (s:ss)  (c:cs)  |     s `member` brackets
                          &&  c `elem` elems brackets
                          &&  c == brackets ! s       = collect ss cs
collect _       (c:_)                                 = Just c

score :: Char -> Int
score ')' =     3
score ']' =    57
score '}' =  1197
score '>' = 25137
score  _  = error "illegal!"

collectScore :: [String] -> Int
collectScore = sum . map score . mapMaybe (collect "")

main = interact $ show . collectScore . lines
