import Control.Applicative
import Control.Monad

import Data.List

import System.IO

import Utils

main :: IO ()
main = do
  input <- filter (/= '\n') <$> readFile "data/8-puzzle-input"
  let width = 25
      height = 6
      dim = width * height
      p1 = part1 dim input
      p2 = map render $ part2 dim input
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2:\n" ++ unlines (slices width p2)
  where render '0' = ' '
        render '1' = '*'


part1 :: Int -> String -> Int
part1 dim input =
  let layers = slices dim input
  in  snd $ minimum $ map (\l -> (zeros l, oneTwo l)) layers
  where zeros l = length $ filter (== '0') l
        oneTwo l = length (filter (== '1') l) * length (filter (== '2') l)

part2 :: Int -> String -> String
part2 dim input =
  let layers = slices dim input
  in foldl1' stack layers
  where stack = zipWith combine

combine :: Char -> Char -> Char
combine '2' c = c
combine c _ = c
