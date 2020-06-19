import Utils

import Control.Monad
import Control.Parallel.Strategies

import Data.Array.Unboxed
import Data.Char
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import Data.List.Split
import Data.List.Extra hiding (splitOn)
import Data.Maybe

import Debug.Trace

type Forest = UArray (Int, Int) Char
type Seen = Map.Map Forest Integer

main :: IO ()
main = do
  putStrLn "Input:"
  forest <- parse <$> readLines []
  let p1 = resourceValue $ step forest Map.empty 10 0
      p2 = resourceValue $ step forest Map.empty 1000000000 0
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2

parse :: [String] -> Forest
parse s = listArray ((1, 1), (n, n)) $ join s
  where n = length s

step :: Forest -> Seen -> Integer -> Integer -> Forest
step f seen n i
  | n == i = f
  | otherwise = case (i -) <$> Map.lookup f seen of
                  Nothing -> step f' (Map.insert f i seen) n (i + 1)
                  Just c  -> step f' Map.empty (n `rem` c) ((i + 1) `rem` c)
  where f' = mapArrWithI (step' f) f

step' :: Forest -> (Int, Int) -> Char -> Char
step' f i c =
  case c of
    '.' -> if nTrees >= 3 then '|' else '.'
    '#' -> if nTrees >= 1 && nLumber >= 1 then '#' else '.'
    '|' -> if nLumber >= 3 then '#' else '|'
  where ns      = map (f !) $ filter (inRange $ bounds f) (neigh' i)
        nTrees  = length $ filter (== '|') ns
        nLumber = length $ filter (== '#') ns

resourceValue :: Forest -> Int
resourceValue (elems -> f) = length (filter (== '|') f) * length (filter (== '#') f)
