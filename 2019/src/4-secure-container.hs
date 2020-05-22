import Control.Applicative

import Data.List

import Utils

main :: IO ()
main = do
  putStrLn "Input:"
  range <- map read . splitString (== '-') <$> getLine
  let digits = [0..9]
      nums = iterate (liftA2 (:) [0..9])  [[]] !! 6
      s = filter (chk range) nums
      p1 = length $ filter (chk range) nums
  putStrLn $ "Part 1: " ++ show p1
  -- putStrLn $ "Part 2: " ++ show p2

chk :: [Int] -> [Int] -> Bool
chk [low, high] num =
  let exp = take 6 $ iterate (*10) 1
      val = sum $ zipWith (*) num exp
      (doub, inc, _) = foldl chk' (False, True, 10) num
  in val >= low && val <= high && doub && inc

chk' (doub, inc, prev) n = (doub || n == prev, inc && n <= prev, n)
