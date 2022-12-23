import           Control.Applicative (liftA2, (<|>))
import           Data.Bifunctor      (first, second)
import           Data.Function       ((&))
import           Data.Map            ((!))
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import           Text.Megaparsec     (some)
import           Utils

coordsParser :: Parser (Set.Set (Int, Int))
coordsParser = Set.fromList <$> some (liftA2 (,) decimal (symbol "," >> decimal))

foldParser :: Parser (Int, Int)
foldParser =
  symbol "fold along" >> (0, ) <$> (symbol "y=" >> decimal) <|> (, 0) <$> (symbol "x=" >> decimal)

main :: IO ()
main = do
  (coords, folds) <- parseFile "data/13-puzzle-input" $ liftA2 (,) coordsParser (some foldParser)
  let p1 = Set.size $ fold coords (head folds)
      final = foldl fold coords folds
      (maxx, maxy) = (maximum (Set.map fst final), maximum (Set.map snd final))
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ unlines (visualize maxx maxy final)

fold coords (0, l) = Set.map (second (fold' l)) coords
fold coords (l, 0) = Set.map (first (fold' l)) coords

fold' l d =
  if d < l
    then d
    else 2 * l - d

visualize :: Int -> Int -> Set.Set (Int, Int) -> [String]
visualize maxx maxy coords =
  map
    (\y ->
       map
         (\x ->
            if (x, y) `Set.member` coords
              then '⬛'
              else '⬜')
         [0 .. maxx])
    [0 .. maxy]
