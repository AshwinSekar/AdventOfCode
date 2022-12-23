import           Data.Array
import           Data.Char
import           Data.List.Split
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Utils

type Region = Array (Int, Int) Int

main :: IO ()
main = do
  putStrLn "Input:"
  depth <- read . filter isDigit <$> getLine
  [x, y] <- map read . splitOn "," . dropWhile (not . isDigit) <$> getLine
  putStrLn $ "Part 1: " ++ show (sum $ (`rem` 3) <$> genRegion (x, y) depth)

genRegion :: (Int, Int) -> Int -> Region
genRegion u depth = region
  where
    l = (0, 0)
    region = array (l, u) [(i, erosion i) | i <- range (l, u)]
    erosion (0, 0) = depth `rem` 20183
    erosion (0, y) = (y * 48271 + depth) `rem` 20183
    erosion (x, 0) = (x * 16807 + depth) `rem` 20183
    erosion p@(x, y)
      | p == u = depth `rem` 20183
      | otherwise = (region ! (x - 1, y) * region ! (x, y - 1) + depth) `rem` 20183
