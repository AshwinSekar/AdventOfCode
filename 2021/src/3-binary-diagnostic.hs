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
      m = (length . head) input
      sums = map ((> n) . sum) $ transpose input
      gamma = map (bool 0 1) sums
      eps = map (bool 1 0) sums
      p1 = bin2dec gamma * bin2dec eps
      f _ [x] _ = [x]
      f cmp nums i = filter (\x -> x !! i == b) nums
        where
          ones = sum $ map (!! i) nums
          zeros = length nums - ones
          b = bool 0 1 (cmp ones zeros)
      [oxygen] = foldl (f (>=)) input [0 .. m]
      [co2] = foldl (f (<)) input [0 .. m]
      p2 = bin2dec oxygen * bin2dec co2
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
