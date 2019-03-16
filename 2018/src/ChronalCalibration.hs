module ChronalCalibration (part1, part2) where

import qualified Data.Set as Set

parseStr :: String -> Integer
parseStr (y:ys)
  | y == '+'  = read ys
  | otherwise = read (y:ys)

findDup :: [Integer] -> Set.Set Integer -> Integer
findDup (x:xs) s = if Set.member x s
                      then x
                      else findDup xs (Set.insert x s)

part1 :: [String] -> Integer
part1 = sum . map parseStr

part2 :: [String] -> Integer
part2 input =
  let infInput = cycle $ map parseStr input
      prefSum = scanl (+) 0 infInput
  in  findDup prefSum Set.empty
