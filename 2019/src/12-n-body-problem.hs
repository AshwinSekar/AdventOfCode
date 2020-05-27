import Control.Applicative
import Control.Monad
import Data.List

import Utils
import Debug.Trace

type MoonState = ([(Int, Int, Int)], [(Int, Int, Int)])

main :: IO ()
main = do
  putStrLn "Input:"
  input <- readLines []
  let posDir = map parse input
      moons = (map fst posDir, map snd posDir)
      moons' = iterate step moons !! 1000
      p1 = totalEnergy moons'
  putStrLn $ "Part 1: " ++ show p1


parse :: String -> ((Int, Int, Int), (Int, Int, Int))
parse s =
  let strs = splitString (== ',') $ filter (/= '>') s
      [x, y, z] = map parse' strs
  in ((x, y, z), (0, 0, 0))
  where parse' = read . (!! 1) . splitString (== '=')


step :: MoonState -> MoonState
step (pos, vs) =
  let as = map (calcA pos) pos
      vs' = zipWith addTuple vs as
      pos' = zipWith addTuple pos vs'
  in (pos', vs')

addTuple (x, y, z) (x', y', z') = (x + x', y + y', z + z')
absTuple (x, y, z) = (abs x, abs y, abs z)

calcA pos (xv, yv, zv) = foldl1' addTuple $ map calcG pos
  where calcG (xv', yv', zv') = (calcG' xv xv', calcG' yv yv', calcG' zv zv')
        calcG' a a' = case compare a a' of
                        LT -> 1
                        EQ -> 0
                        GT -> -1

totalEnergy :: MoonState -> Int
totalEnergy (pos, vs) = sum $ zipWith energy (map absTuple pos) (map absTuple vs)

energy :: (Int, Int, Int) -> (Int, Int, Int) -> Int
energy (x, y, z) (xv, yv, zv) = (x + y + z) * (xv + yv + zv)
