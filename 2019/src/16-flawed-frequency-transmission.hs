import Control.Applicative
import Control.Monad
import Data.Char
import Data.List

import Utils
import Debug.Trace


main :: IO ()
main = do
  putStrLn "Input:"
  input <- getLine
  let signal = map digitToInt input
      ffts = iterate fft signal
      p1 = join $ map show (ffts !! 100)
  putStrLn $ "Part 1: " ++ take 8 p1
  -- putStrLn $ "Part 2: " ++ show p2


fft :: [Int] -> [Int]
fft signal =
  let elems = map (compute signal) [1..]
      digits = map (`rem` 10) elems
  in take (length signal) $ map abs digits

withPattern :: Int -> Int -> Int -> Int
withPattern i s j = s * ([0, 1, 0, -1] !! ((j `div` i) `rem` 4))

compute :: [Int] -> Int -> Int
compute signal i = sum $ zipWith (withPattern i) signal [1..]
