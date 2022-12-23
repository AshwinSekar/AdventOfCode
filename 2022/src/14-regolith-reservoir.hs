import           Control.Applicative  (Alternative ((<|>)), liftA2)
import           Control.Monad        (liftM2)
import           Data.Char            (isAsciiUpper, ord)
import           Data.Complex         (Complex ((:+)), imagPart, realPart)
import           Data.Foldable        (foldlM)
import           Data.Function        ((&))
import           Data.Functor         (($>))
import qualified Data.Heap            as PQ
import           Data.Ix              (Ix (range))
import           Data.List            (find, intersect, sort)
import           Data.Map             ((!), (!?))
import qualified Data.Map             as Map
import           Data.Maybe           (mapMaybe)
import qualified Data.Set             as Set
import           Text.Megaparsec      (MonadParsec (try), choice, some)
import           Text.Megaparsec.Char (char, newline, printChar)
import           Utils                (Parser, count, decimal, fourDirs, getFile, gridMap, lexeme,
                                       parseFile, psum, slices, symbol, word)

data Block
  = Rock
  | Sand
  deriving (Eq, Show)

type Grid = Map.Map (Integer, Integer) Block

start :: (Integer, Integer)
start = (500, 0)

genline :: (Ord a1, Ord a2, Enum a1, Enum a2) => (a1, a2) -> (a1, a2) -> [(a1, a2)]
genline (a, b) (c, d) = liftM2 (,) [(min a c) .. (max a c)] [(min b d) .. (max b d)]

rocksParser :: Parser [(Integer, Integer)]
rocksParser = do
  coords <- some $ try (liftA2 (,) (decimal <* char ',') (decimal <* symbol "->"))
  coords' <- (\l -> coords ++ [l]) <$> liftA2 (,) (decimal <* char ',') decimal
  pure $ concat $ zipWith genline coords' (drop 1 coords')

dropSand :: Integer -> (Integer, Integer) -> Grid -> Integer -> Either Integer Grid
dropSand h pos@(x, y) g i
  | y > h = Left i
  | otherwise =
    case (chk (x - 1, y + 1), chk (x, y + 1), chk (x + 1, y + 1)) of
      (_, False, _) -> dropSand h (x, y + 1) g i
      (False, _, _) -> dropSand h (x - 1, y + 1) g i
      (_, _, False) -> dropSand h (x + 1, y + 1) g i
      _             -> Right $ Map.insert pos Sand g
  where
    chk pos = Map.member pos g

fillFloor :: Integer -> (Integer, Integer) -> Grid -> Integer -> Either Integer Grid
fillFloor h pos@(x, y) g i
  | chk pos = Left i
  | otherwise =
    case (chk (x - 1, y + 1), chk (x, y + 1), chk (x + 1, y + 1)) of
      (_, False, _) -> fillFloor h (x, y + 1) g i
      (False, _, _) -> fillFloor h (x - 1, y + 1) g i
      (_, _, False) -> fillFloor h (x + 1, y + 1) g i
      _             -> Right $ Map.insert pos Sand g
  where
    chk pos@(x, y) = Map.member pos g || y == h

main :: IO ()
main = do
  rocks <- parseFile "data/14-puzzle-input" $ concat <$> some rocksParser
  let abyss = maximum $ map snd rocks
      floor = abyss + 2
      objects = Map.fromList $ map (, Rock) rocks
      Left p1 = foldlM (dropSand abyss start) objects [0,1 ..]
      Left p2 = foldlM (fillFloor floor start) objects [0,1 ..]
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
