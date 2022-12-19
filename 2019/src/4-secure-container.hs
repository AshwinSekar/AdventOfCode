import Control.Applicative
import Data.List
import Utils

main :: IO ()
main = do
  putStrLn "Input:"
  range <- map read . splitString (== '-') <$> getLine
  let digits = [0 .. 9]
      nums = iterate (liftA2 (:) [0 .. 9]) [[]] !! 6
      incNums = filter (chk range) nums
      p1 = length $ filter (chkGroup (>= 2)) incNums
      p2 = length $ filter (chkGroup (== 2)) incNums
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2

chk :: [Int] -> [Int] -> Bool
chk [low, high] num =
  let exp = take 6 $ iterate (* 10) 1
      val = sum $ zipWith (*) num exp
      (inc, _) = foldl chkInc (True, 10) num
   in val >= low && val <= high && inc

chkGroup :: (Int -> Bool) -> [Int] -> Bool
chkGroup p num = any (p . length) $ group num

chkInc (inc, prev) n = (inc && n <= prev, n)
