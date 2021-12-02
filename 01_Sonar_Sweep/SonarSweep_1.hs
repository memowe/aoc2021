module SonarSweep_1 where

main :: IO ()
main = interact $ show . sonarSweep . map read . lines
  where sonarSweep :: [Int] -> Int
        sonarSweep  = sum . map fromEnum . goingUp
        goingUp     = zipWith (<) <*> tail
