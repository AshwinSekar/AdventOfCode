import Control.Applicative
import Control.Monad
import Data.Array
import Data.Char
import Data.Function
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Debug.Trace
import Utils

type Bugs = Map.Map (Int, Int) Int

main :: IO ()
main = do
  putStrLn "Input:"
  input <- readLines []
  let bugs = concatMap parse input
      bugs' = parseBug input
      l = listArray ((0, 0), (4, 4)) bugs
      p1 = rating $ findRepeat l (Set.singleton l)
      p2 =
        iterate step bugs'
          & (!! 200)
          & sum
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2

parseBug :: [String] -> Bugs
parseBug inputs =
  concat inputs
    & foldl build (1, empty)
    & snd
  where
    build (13, bugs) _ = (14, bugs)
    build (i, bugs) '.' = (i + 1, Map.insert (i, 0) 0 bugs)
    build (i, bugs) '#' = (i + 1, Map.insert (i, 0) 1 bugs)
    empty = Map.fromList [((i, z), 0) | i <- grid, z <- [-100 .. 100]]
    grid = delete 13 [1 .. 25]

step :: Bugs -> Bugs
step bugs = Map.mapWithKey (transform' bugs) bugs

transform' bugs (i, z) 0
  | (adjs bugs (i, z) == 1) || (adjs bugs (i, z) == 2) = 1
  | otherwise = 0
transform' bugs (i, z) 1
  | adjs bugs (i, z) == 1 = 1
  | otherwise = 0

adjs bugs (i, z) =
  adjs' bugs (i, z)
    & mapMaybe (`Map.lookup` bugs)
    & sum

adjs' bugs (1, z) = [(8, z - 1), (12, z - 1), (2, z), (6, z)]
adjs' bugs (5, z) = [(8, z - 1), (14, z - 1), (4, z), (10, z)]
adjs' bugs (21, z) = [(12, z - 1), (18, z - 1), (16, z), (22, z)]
adjs' bugs (25, z) = [(14, z - 1), (18, z - 1), (20, z), (24, z)]
adjs' bugs (8, z) = [(3, z), (7, z), (9, z)] ++ map (,z + 1) [1 .. 5]
adjs' bugs (12, z) = [(7, z), (11, z), (17, z)] ++ map (\i -> (5 * i + 1, z + 1)) [0 .. 4]
adjs' bugs (14, z) = [(9, z), (15, z), (19, z)] ++ map (\i -> (5 * i, z + 1)) [1 .. 5]
adjs' bugs (18, z) = [(17, z), (19, z), (23, z)] ++ map (,z + 1) [21 .. 25]
adjs' bugs (i, z)
  | i < 5 = [(8, z - 1), (i - 1, z), (i + 1, z), (i + 5, z)]
  | i > 21 = [(18, z - 1), (i - 1, z), (i + 1, z), (i - 5, z)]
  | i `rem` 5 == 1 = [(12, z - 1), (i - 5, z), (i + 5, z), (i + 1, z)]
  | i `rem` 5 == 0 = [(14, z - 1), (i - 5, z), (i + 5, z), (i - 1, z)]
  | otherwise = [(i - 5, z), (i + 5, z), (i - 1, z), (i + 1, z)]

parse :: String -> [Int]
parse = map parse'
  where
    parse' '.' = 0
    parse' '#' = 1

findRepeat l s =
  let l' = advance l
   in if Set.member l' s
        then l'
        else findRepeat l' (Set.insert l' s)

rating l = foldr (\x y -> 2 * y + x) 0 $ elems l

advance l =
  let is = indices l
      is' = map (transform l) is
   in listArray ((0, 0), (4, 4)) is'

transform l p =
  case (l ! p, adjBugs l p) of
    (0, 1) -> 1
    (0, 2) -> 1
    (1, 1) -> 1
    _ -> 0

adjBugs l (i, j) = length $ filter (== 1) (map bug dirs)
  where
    dirs = [(i, j - 1), (i, j + 1), (i + 1, j), (i - 1, j)]
    bug p = case p of
      (-1, _) -> 0
      (5, _) -> 0
      (_, -1) -> 0
      (_, 5) -> 0
      _ -> l ! p

showBugs l = unlines $ slices 5 (map iParse (elems l))
  where
    iParse 0 = '.'
    iParse 1 = '#'
