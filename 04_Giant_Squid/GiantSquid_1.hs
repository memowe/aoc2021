import Data.Either
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Function

newtype Position a = Position { value :: Either a a }
newtype Board a    = Board    { rows :: [[Position a]] }

instance Show a => Show (Position a) where
  show (Position (Left p))  = " " ++ show p ++ " "
  show (Position (Right s)) = "[" ++ show s ++ "]"

instance Show a => Show (Board a) where
  show = unlines . map (unwords . (show <$>)) . rows

createBoard :: [[a]] -> Board a
createBoard = Board . map (Position . Left <$>)

hasWon :: Board a -> Bool
hasWon = ((||) `on` any rights) <$> rows <*> transpose . rows
  where rights = all (isRight . value)

select :: Eq a => a -> Board a -> Board a
select val = Board . map (spos <$>) . rows
  where spos p@(Position (Left v))
          | v == val  = Position (Right v)
          | otherwise = p
        spos p = p

firstWinningScore :: (Eq a, Num a) => [a] -> [Board a] -> a
firstWinningScore input boards =
  let moves       = scanl draw (boards, 0) input
      (bs, last)  = fromJust $ find (any hasWon . fst) moves
  in  last * sum (unmarked $ fromJust $ find hasWon bs)
  where draw (bs,_) i = (map (select i) bs, i)
        unmarked      = concatMap ((fromLeft 0 . value) <$>) . rows

parse :: String -> ([Int], [Board Int])
parse = ((,) <$> toInput <*> toBoards) . splitWhen null . lines
  where toInput   = map read . splitOn "," . head . head
        toBoards  = map (createBoard . map ((read <$>) . words)) . tail

main :: IO ()
main = interact $ show . uncurry firstWinningScore . parse
