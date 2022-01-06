import           Utils

import           Control.Applicative (liftA2, (<|>))

import           Data.Function       ((&))
import           Data.List           (find)
import           Data.Map            ((!), (!?))
import qualified Data.Map            as Map
import           Data.Maybe          (mapMaybe)
import qualified Data.Set            as Set

tareaParser :: Parser ((Int, Int), (Int, Int))
tareaParser = do
  x0 <- symbol "target area: x=" *> signed
  x1 <- symbol ".." *> signed
  y0 <- symbol ", y=" *> signed
  y1 <- symbol ".." *> signed
  pure ((x0, x1), (y0, y1))

main :: IO ()
main = do
  ((x0, x1), (y0, y1)) <- parseFile "data/17-puzzle-input" tareaParser
  let start = (0, 0)
      (xlow, _) = invTriRange (x0, x1)
      tri n = n * (n + 1) `div` 2
      invTriRange (l, l') =
        ((isqrt (1 + 8 * l) + 1) `div` 2, isqrt (1 + 8 * l') `div` 2)
      validC x c = x0 <= tri x - tri c && tri x - tri c <= x1
      xlows =
        mapMaybe (\x -> (x, ) <$> find (validC x) [0 .. (x - 1)]) [xlow .. x1]
      xhighs =
        mapMaybe
          (\x -> (x, ) <$> find (validC x) [(x - 1),(x - 2) .. 0])
          [xlow .. x1]
  putStrLn $ "Part 1: " ++ show (tri (-y0 - 1))
  -- putStrLn $ "Part 2: " ++ show (score (head init) p2)
