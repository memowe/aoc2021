import Data.List.Split
import Data.Function

parse :: String -> ([String], [String])
parse = (((,) `on` words . head) <*> tail) . splitOn " | "

main = interact $ show . parse
