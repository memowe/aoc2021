import Data.List
import Data.Function

data Result = Legal | Incomplete String | Illegal Char

collect :: String -> String -> Result
collect ""      ""        = Legal
collect s       ""        = Incomplete $ rev s
collect s       ('(':cs)  = collect ('(':s) cs
collect s       ('[':cs)  = collect ('[':s) cs
collect s       ('{':cs)  = collect ('{':s) cs
collect s       ('<':cs)  = collect ('<':s) cs
collect ('(':s) (')':cs)  = collect s cs
collect ('[':s) (']':cs)  = collect s cs
collect ('{':s) ('}':cs)  = collect s cs
collect ('<':s) ('>':cs)  = collect s cs
collect _       (c:_)     = Illegal c

rev :: String -> String
rev = map r where r '(' = ')'; r '[' = ']'; r '{' = '}'; r '<' = '>'

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
