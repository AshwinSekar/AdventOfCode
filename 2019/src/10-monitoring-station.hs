import Control.Applicative
import Control.Monad
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map

import Utils

data Angle = Zero | Atan Int Int deriving (Show, Eq, Ord)

main :: IO ()
main = do
  putStrLn "Input:"
  input <- readLines []
  let grid = zipWith (\s i -> zipWith (\c j -> (c, j, i)) s [0..]) input [0..]
      asts = map dropFst $ filter ((== '#') . fst3) $ concat grid
      p1 = findMonitor asts
  putStrLn $ "Part 1: " ++ show p1
  -- putStrLn $ "Part 2: " ++ show p2
  where fst3 (a, _, _) = a
        dropFst (_, a, b) = (a, b)


findMonitor :: [(Int, Int)] -> (Int, (Int, Int))
findMonitor asts = maximum $ map computeDiff asts
  where computeDiff coord = ((length . diffSet asts) coord, coord)

diffSet asts (x, y) = Set.fromList $ map diffAngle (filter (/= (x, y)) asts)
  where diffAngle (x', y') = (x - x') % (y - y')

x % y = Atan (y `quot` d) (x `quot` d)
  where d = gcd x y
