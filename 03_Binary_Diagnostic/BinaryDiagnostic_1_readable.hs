import Data.List
import Data.Function

main = interact (show . combine . democracy . map toBools . lines)

  where
        toBools :: String -> [Bool]
        toBools = map (== '1')

        democracy :: [[Bool]] -> [Bool]
        democracy = map most . transpose

        most :: [Bool] -> Bool
        most bs = sum (map fromEnum bs) > (length bs `div` 2)
        -- most = (>) <$> sum . map fromEnum <*> (`div` 2) . length

        combine :: [Bool] -> Int
        combine bs = toInt bs * toInt (map not bs)
        -- combine = ((*) `on` toInt) <*> map not

        toInt :: [Bool] -> Int
        toInt bs = foldl (\i b -> i*2 + fromEnum b) 0 bs
        -- toInt = foldl ((. fromEnum) . (+) . (*2)) 0
