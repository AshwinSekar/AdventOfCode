import           Utils

import           Control.Applicative (liftA2, (<|>))

import           Data.Bool           (bool)
import           Data.Char           (digitToInt)
import           Data.Function       ((&))
import           Data.List           (transpose)
import           Data.Map            ((!))
import qualified Data.Map            as Map
import qualified Data.Set            as Set

main :: IO ()
main = do
  input <- map (map digitToInt) <$> getFile "data/3-puzzle-input"
  let n = length input `div` 2
      sums = map ((> n) . sum) $ transpose input
      gamma = bin2dec $ map (bool 0 1) sums
      eps = bin2dec $ map (bool 1 0) sums
      p1 = gamma * eps
  putStrLn $ "Part 1: " ++ show p1
  -- putStrLn $ "Part 2: " ++ show p2
