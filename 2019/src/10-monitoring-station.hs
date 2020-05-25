import Control.Applicative
import Control.Monad
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map

import Utils

data Angle = ATan Int Int deriving (Eq, Show, Read)

main :: IO ()
main = do
  putStrLn "Input:"
  input <- readLines []
  let grid = zipWith (\s i -> zipWith (\c j -> (c, j, i)) s [0..]) input [0..]
      asts = map dropFst $ filter ((== '#') . fst3) $ concat grid
      p1@(_, monitor) = findMonitor asts
      (x, y) = vaporize monitor asts !! 199
      p2 = x * 100 + y
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
  where fst3 (a, _, _) = a
        dropFst (_, a, b) = (a, b)


findMonitor :: [(Int, Int)] -> (Int, (Int, Int))
findMonitor asts = maximum $ map computeDiff asts
  where computeDiff coord = ((length . diffSet asts) coord, coord)

diffSet :: [(Int, Int)] -> (Int, Int) -> Set.Set Angle
diffSet asts (x, y) = Set.fromList $ map diffAngle (filter (/= (x, y)) asts)
  where diffAngle (x', y') = (x - x') % (y - y')

vaporize monitor asts = vaporize' monitor asts'
  where asts' = filter (/= monitor) asts

vaporize' (x, y) asts =
  let vecs = map diffVec asts
      vecByTheta = groupBy angEq $ sort vecs
      vecByThetaAdj = map adjust vecByTheta
  in map snd $ sort (concat vecByThetaAdj)
  where diffVec (x', y') = (makeVec (x' - x) (y - y'), (x', y'))
        angEq ((a1,_), _) ((a2,_), _) = a1 == a2
        adjust vs = zipWith adjusti vs [0..]
        adjusti ((theta, r), (x, y)) i = (atan' theta + 2 * pi * i, (x, y))

(%) :: Int -> Int -> Angle
x % y = ATan (y `quot` d) (x `quot` d)
  where d = gcd x y

atan' :: Angle -> Double
atan' (ATan y x) = pos $ atan2 (fromIntegral x) (fromIntegral y)
  where pos a | a < 0 = a + 2 * pi
              | otherwise = a

makeVec :: Int -> Int -> (Angle, Double)
makeVec x y = (x % y, sqrt $ fromIntegral (x * x + y * y))

instance Ord Angle where
  compare a1 a2 = compare (atan' a1) (atan' a2)
