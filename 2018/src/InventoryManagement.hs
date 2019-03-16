module InventoryManagement (part1, part2) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List
import Data.Maybe

letterCount :: String -> (Int, Int)
letterCount s =
  let counts = foldr (\k -> Map.insertWith (+) k 1) Map.empty s
      twos = Map.filter (==2) counts
      threes = Map.filter (==3) counts
  in (fromEnum $ Map.size twos > 0, fromEnum $ Map.size threes > 0)

withoutOneLists :: [a] -> [[a]]
withoutOneLists [] = []
withoutOneLists (x:xs) = xs : map (x :) (withoutOneLists xs)

findDuplicate :: Ord a => [[a]] -> [Set.Set a] -> a
findDuplicate (words:remaining) sets =
  let inserted = zipWith insertMaybe words sets
      duplicate = find (isJust . fst) inserted
      sets' = map snd inserted
  in if isJust duplicate
       then fromJust $ fst $ fromJust duplicate
       else findDuplicate remaining sets'
  where insertMaybe w s = if Set.member w s then (Just w, s) else (Nothing, Set.insert w s)

part1 :: [String] -> Int
part1 input =
  let counts = map letterCount input
      (total2, total3) = foldl (\(a, b) (c, d) -> (a + c, b + d)) (0, 0) counts
  in total2 * total3

part2 :: [String] -> String
part2 input =
  let withoutOneAll = map withoutOneLists input
      sets = repeat Set.empty
  in findDuplicate withoutOneAll sets
