import Data.List.Split
import Data.Function

data Seven a = SSeg a a a a a a a

instance Functor Seven where
  fmap f (SSeg x1 x2 x3 x4 x5 x6 x7) =
    SSeg (f x2) (f x2) (f x3) (f x4) (f x5) (f x6) (f x7)

instance Applicative Seven where
  pure x = SSeg x x x x x x x
  (SSeg f1 f2 f3 f4 f5 f6 f7) <*> (SSeg x1 x2 x3 x4 x5 x6 x7) = 
    SSeg (f1 x1) (f2 x2) (f3 x3) (f4 x4) (f5 x5) (f6 x6) (f7 x7)

fromList :: [a] -> Seven a
fromList [x1,x2,x3,x4,x5,x6,x7] = SSeg x1 x2 x3 x4 x5 x6 x7
fromList _ = error "invalid!"

toList :: Seven a -> [a]
toList (SSeg x1 x2 x3 x4 x5 x6 x7) = [x1,x2,x3,x4,x5,x6,x7]

type SevenSegment = Seven Bool

str2Num :: String -> SevenSegment
str2Num = fromList . map (== '1')

num :: Int -> SevenSegment
num 0 = str2Num "1110111"
num 1 = str2Num "0010010"
num 2 = str2Num "1011101"
num 3 = str2Num "1011011"
num 4 = str2Num "0111010"
num 5 = str2Num "1101011"
num 6 = str2Num "1101111"
num 7 = str2Num "1010010"
num 8 = str2Num "1111111"
num 9 = str2Num "1111011"
num _ = error "invalid!"

parse :: String -> ([String], [String])
parse = (((,) `on` words . head) <*> tail) . splitOn " | "
