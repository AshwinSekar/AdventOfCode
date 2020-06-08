import Control.Applicative
import Control.Monad

import Data.Array
import Data.Char
import Data.List
import qualified Data.Set as Set

import Utils
import Debug.Trace


main :: IO ()
main = do
  putStrLn "Input:"
  input <- readLines []
  let bugs = concatMap parse input
      l = listArray ((0, 0), (4, 4)) bugs
      l' = findRepeat l (Set.singleton l)
      p1 = rating l'
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ showBugs l'

parse :: String -> [Int]
parse = map parse'
  where parse' '.' = 0
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
  where dirs = [(i, j - 1), (i, j + 1), (i + 1, j), (i - 1, j)]
        bug p = case p of
          (-1, _) -> 0
          (5, _) -> 0
          (_, -1) -> 0
          (_, 5) -> 0
          _ -> l ! p

showBugs l = unlines $ slices 5 (map iParse (elems l))
  where iParse 0 = '.'
        iParse 1 = '#'
