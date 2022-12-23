import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Tuple.Extra
import           Debug.Trace
import           Utils

type MoonState = ([(Int, Int, Int)], [(Int, Int, Int)])

main :: IO ()
main = do
  putStrLn "Input:"
  input <- readLines []
  let posDir = map parse input
      moons = (map fst posDir, map snd posDir)
      p1 = totalEnergy $ iterate step moons !! 1000
      v = map parseVec input
      moons' = [map fst3 v, map snd3 v, map thd3 v]
      p2 = foldl1' lcm $ map period moons'
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2

parse :: String -> ((Int, Int, Int), (Int, Int, Int))
parse s =
  let strs = splitString (== ',') $ filter (/= '>') s
      [x, y, z] = map parse' strs
   in ((x, y, z), (0, 0, 0))

parseVec :: String -> (Integer, Integer, Integer)
parseVec s =
  let strs = splitString (== ',') $ filter (/= '>') s
      [x, y, z] = map parse' strs
   in (x, y, z)

parse' :: (Integral a, Read a) => String -> a
parse' = read . (!! 1) . splitString (== '=')

step :: MoonState -> MoonState
step (pos, vs) =
  let as = map (calcA pos) pos
      vs' = zipWith addTuple vs as
      pos' = zipWith addTuple pos vs'
   in (pos', vs')

step' :: ([Integer], [Integer]) -> ([Integer], [Integer])
step' (pos, vs) =
  let as = map (calcA' pos) pos
      vs' = zipWith (+) vs as
      pos' = zipWith (+) pos vs'
   in (pos', vs')

addTuple (x, y, z) (x', y', z') = (x + x', y + y', z + z')

absTuple (x, y, z) = (abs x, abs y, abs z)

calcA pos (xv, yv, zv) = foldl1' addTuple $ map calcG pos
  where
    calcG (xv', yv', zv') = (calcG' xv xv', calcG' yv yv', calcG' zv zv')

calcA' pos xs = sum $ map (calcG' xs) pos

calcG' a a' =
  case compare a a' of
    LT -> 1
    EQ -> 0
    GT -> -1

totalEnergy :: MoonState -> Int
totalEnergy (pos, vs) = sum $ zipWith energy (map absTuple pos) (map absTuple vs)

energy :: (Int, Int, Int) -> (Int, Int, Int) -> Int
energy (x, y, z) (xv, yv, zv) = (x + y + z) * (xv + yv + zv)

period :: [Integer] -> Integer
period pos = 2 * halfP
  where
    vs = repeat 0
    halfP = period' pos 1 $ step' (pos, vs)

period' :: [Integer] -> Integer -> ([Integer], [Integer]) -> Integer
period' oPos x state@(pos, vs) =
  if all (== 0) vs
    then x
    else period' oPos (x + 1) $ step' state
