import Data.Functor ((<&>))
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Tuple.Extra
import Data.Void
import Text.Megaparsec (many, someTill)
import Text.Megaparsec.Char (newline, printChar)
import Utils

type Grid a = Map.Map a Bool

class Point a where
  neighbors :: a -> [a]

instance Point (Integer, Integer, Integer) where
  neighbors coord = map (psum3 coord) eightDirs3D

instance Point (Integer, Integer, Integer, Integer) where
  neighbors coord = map (psum4 coord) eightDirs4D

parser :: Parser [[Bool]]
parser = many (map (== '#') <$> someTill printChar newline)

boot :: (Point a, Ord a) => Grid a -> Grid a
boot grid = Map.fromSet active newCoords
  where
    gc c = Map.findWithDefault False c grid
    newCoords = Set.fromList (concatMap neighbors $ Map.keys grid)
    active coord
      | gc coord = activeN == 2 || activeN == 3
      | otherwise = activeN == 3
      where
        activeN = count gc $ neighbors coord

main :: IO ()
main = do
  (p1, p2) <-
    parseFile "data/17-puzzle-input" parser
      <&> gridMap
      <&> Map.mapKeys (\(x, y) -> (x, y, 0 :: Integer)) &&& Map.mapKeys (\(x, y) -> (x, y, 0 :: Integer, 0 :: Integer))
      <&> Map.elems . (!! 6) . iterate boot *** Map.elems . (!! 6) . iterate boot
      <&> both (count id)
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
