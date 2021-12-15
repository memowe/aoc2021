import Data.Char
import Data.Tuple
import Data.Bifunctor
import qualified Data.Set as S
import Text.ParserCombinators.ReadP

type Origami  = S.Set (Int, Int)
data FoldDir  = FoldLeft | FoldUp
data Fold     = Fold FoldDir Int

applyFold :: Origami -> Fold -> Origami
applyFold o (Fold FoldUp i)   =
  S.map swap $ applyFold (S.map swap o) (Fold FoldLeft i)
applyFold o (Fold FoldLeft i) =
  let left  = S.filter ((< i) . fst) o
      right = S.filter ((> i) . fst) o
      maxX  = maximum $ map fst $ S.toList right
  in  left `S.union` S.map (first (maxX -)) right

parser :: ReadP (Origami, [Fold])
parser = do pairs <- many pair
            char '\n'
            folds <- many foldP
            return (S.fromList pairs, folds)
  where num   = read <$> munch1 isDigit
        pair  = do  x <- num
                    y <- between (char ',') (char '\n') num
                    return (x,y)
        foldP = do  string "fold along "
                    c <- char 'x' +++ char 'y'
                    let dir = if c == 'x' then FoldLeft else FoldUp
                    i <- between (char '=') (char '\n') num
                    return $ Fold dir i

main = interact $ show . uncurry countFstFold . parse
  where parse         = fst . last . readP_to_S parser
        countFstFold  = (. head) . (S.size .) . applyFold
