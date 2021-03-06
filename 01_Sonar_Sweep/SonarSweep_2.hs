import Data.List

main = interact $ show . sonarSweep . map read . lines
  where sonarSweep :: [Int] -> Int
        sonarSweep  = sum . map fromEnum . goingUp . map sum . window 3
        goingUp     = zipWith (<) <*> tail
        window size = filter ((== size) . length) . map (take size) . tails
