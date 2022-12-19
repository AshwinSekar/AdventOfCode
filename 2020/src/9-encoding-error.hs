import Control.Applicative (liftA2)
import Data.Functor ((<&>))
import Data.List (find)
import Data.Map ((!))
import qualified Data.Map as Map
import Utils

findInvalid :: Int -> Map.Map Int Int -> Int
findInvalid i nums =
  case find (== (nums ! i)) sums of
    Nothing -> nums ! i
    Just x -> findInvalid (i + 1) nums
  where
    last25 = map (nums !) [i - 25 .. i - 1]
    sums = liftA2 (\x y -> if x == y then -1 else x + y) last25 last25

conSum :: Int -> Map.Map Int Int -> Int -> Int -> Int -> Int
conSum t nums i j cur
  | cur == t = minimum contig + maximum contig
  | cur < t = conSum t nums i (j + 1) (cur + y)
  | cur > t = conSum t nums (i + 1) j (cur - x)
  where
    x = nums ! i
    y = nums ! j
    contig = map (nums !) [i .. j]

main :: IO ()
main = do
  input <-
    getFile "data/9-puzzle-input"
      <&> map read
      <&> zip [1 ..]
      <&> Map.fromList
  let p1 = findInvalid 26 input
      p2 = conSum p1 input 1 2 (input ! 1)
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
