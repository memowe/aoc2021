module SonarSweep_1 where

main :: IO ()
main = interact (show . sonarSweep . map read . lines)
  where sonarSweep :: [Int] -> Int
        sonarSweep  = length . filter id . goingUp
        goingUp     = (False :) . (zipWith (<) <*> tail)
