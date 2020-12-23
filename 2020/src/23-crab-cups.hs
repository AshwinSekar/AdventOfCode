import Utils

import Control.Monad (join)
import Data.Function ((&))
import Data.List (unfoldr)
import Data.HashMap.Lazy ((!))
import qualified Data.HashMap.Lazy as Map

play :: Int -> (Map.HashMap Int Int, Int) -> (Map.HashMap Int Int, Int)
play m (cups, cur) = (Map.insert cur d cups
                        & Map.insert c e
                        & Map.insert n a
                      , d)
  where select 0 = select m
        select n = if n `elem` [a,b,c] then select (n - 1) else n
        a = cups ! cur
        b = cups ! a
        c = cups ! b
        d = cups ! c
        n = select (cur - 1)
        e = cups ! n

main :: IO ()
main = do
  let cups = [8, 5, 3, 1, 9, 2, 6, 4, 7]
      cups' = cups ++ [10..1000000]
      (p1, _) = iterate (play 9) (init cups)  !! 100
      (p2, _) = iterate (play 1000000) (init cups')  !! 10000000
  putStrLn $ "Part 1: " ++ join (map show $ unfoldr (\c -> if c == 1 then Nothing else Just (c, p1 ! c)) (p1 ! 1))
  putStrLn $ "Part 2: " ++ show (p2 ! 1 * (p2 ! (p2 ! 1)))
  where init l@(x:xs) = (Map.fromList $ zip l (xs ++ [x]), x)
