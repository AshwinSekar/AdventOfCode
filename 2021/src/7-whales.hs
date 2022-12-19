import           Control.Applicative (liftA2, (<|>))
import           Data.Function       ((&))
import           Data.Map            ((!))
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import           Text.Megaparsec     (sepBy1)
import           Utils

main :: IO ()
main = do
  crabs <- parseFile "data/7-puzzle-input" $ decimal `sepBy1` symbol ","
  let start = sum crabs `div` length crabs
      dist x y = abs (y - x)
      dist' x y = n * (n + 1) `div` 2
        where
          n = abs (y - x)
      go f x =
        case (l < r, d < l && d < r) of
          (_, True) -> calc x
          (True, _) -> go f (x - 1)
          _         -> go f (x + 1)
        where
          calc y = sum $ map (f y) crabs
          [l, d, r] = calc <$> [x - 1, x, x + 1]
  putStrLn $ "Part 1: " ++ show (go dist start)
  putStrLn $ "Part 2: " ++ show (go dist' start)
