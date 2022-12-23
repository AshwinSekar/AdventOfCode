import           Control.Monad
import           Control.Parallel.Strategies
import           Data.Char
import           Data.Foldable
import           Data.Function
import           Data.List
import qualified Data.Map                    as Map
import           Data.Maybe
import qualified Data.Set                    as Set
import           Debug.Trace
import           Utils

type Grid = Map.Map (Integer, Integer) Integer

(|!) :: Grid -> (Integer, Integer) -> Integer
grid |! (i, j)
  | i < 1 || j < 1 || i > 300 || j > 300 = 0
  | otherwise = grid Map.! (i, j)

main :: IO ()
main = do
  putStrLn "Input:"
  gsn <- read <$> getLine
  let p1 = maxFuel gsn
      p2 = maxFuelSquare gsn
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2

maxFuel :: Integer -> (Integer, Integer)
maxFuel gsn =
  [(i, j) | i <- [1 .. 298], j <- [1 .. 298]] & map (\s -> (compute3x3Fuel gsn s, s)) & maximum &
  snd

compute3x3Fuel :: Integer -> (Integer, Integer) -> Integer
compute3x3Fuel gsn cell =
  map (compute gsn . (^+ cell)) ([(i, j) | i <- [0 .. 2], j <- [0 .. 2]]) & sum

maxFuelSquare :: Integer -> (Integer, Integer, Integer)
maxFuelSquare gsn =
  [(x, y) | x <- [1 .. 300], y <- [1 .. 300]] &
  concatMap (\(x, y) -> map (x, y, ) [1 .. (min (301 - x) (301 - y))]) &
  parMap rpar (\s -> (computeSquare rects s, s)) &
  maximum &
  snd
  where
    cells =
      [(i, j) | i <- [1 .. 300], j <- [1 .. 300]] & map (\s -> (s, compute gsn s)) & Map.fromList
    rects = computeRect cells (300, 300) Map.empty

computeRect :: Grid -> (Integer, Integer) -> Grid -> Grid
computeRect cells (_, 0) r = r
computeRect cells (0, y) r = computeRect cells (300, y - 1) r
computeRect cells c@(x, y) r = computeRect cells (x - 1, y) r'
  where
    r' = Map.insert (x, y) rect r
    rect = r |! (x + 1, y) + r |! (x, y + 1) - r |! (x + 1, y + 1) + cells Map.! c

computeSquare :: Grid -> (Integer, Integer, Integer) -> Integer
computeSquare r (x, y, z) = r |! (x, y) - r |! (x + z, y) - r |! (x, y + z) + r |! (x + z, y + z)

compute :: Integer -> (Integer, Integer) -> Integer
compute gsn (x, y) = (rackId * y + gsn) * rackId `div` 100 `rem` 10 - 5
  where
    rackId = x + 10
