import Data.Either
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

cols :: Board a -> [[Position a]]
cols = transpose . rows

createBoard :: [[a]] -> Board a
createBoard = Board . map (Position . Left <$>)

hasWon :: Board a -> Bool
hasWon = ((||) `on` any rights) <$> rows <*> cols
  where rights = all (isRight . value)

selectPos :: Eq a => a -> Position a -> Position a
selectPos val p@(Position (Left v))
  | v == val  = Position (Right v)
  | otherwise = p
selectPos _ p = p

select :: Eq a => a -> Board a -> Board a
select val = Board . map (map (selectPos val)) . rows



parse :: String -> ([Int], [Board Int])
parse s = let (is:bs) = linesBy null (lines s)
              input   = map read (splitOn "," (head is))
              boards  = map (createBoard . map ((read <$>) . words)) bs
          in  (input, boards)

main = interact $ show . parse
