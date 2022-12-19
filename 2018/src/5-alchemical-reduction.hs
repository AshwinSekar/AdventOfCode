import Control.Monad
import Data.Char
import Data.Foldable
import Data.Function
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Debug.Trace
import System.IO
import Utils

main :: IO ()
main = do
  p1 <- withFile "data/5-puzzle-input" ReadMode scanPolymer
  p2 <- withFile "data/5-puzzle-input" ReadMode improvePolymer
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2

scanPolymer :: Handle -> IO Int
scanPolymer h = scanPolymer' h [] 0

scanPolymer' :: Handle -> String -> Int -> IO Int
scanPolymer' h seen n = do
  c <- hGetChar h
  scan c seen
  where
    scan '\n' _ = return n
    scan c [] = scanPolymer' h [c] 1
    scan c (x : xs)
      | eqOpCase c x = scanPolymer' h xs (n - 1)
      | otherwise = scanPolymer' h (c : seen) (n + 1)

improvePolymer :: Handle -> IO Int
improvePolymer h =
  map (,(0, [])) ['A' .. 'Z']
    & Map.fromList
    & improve h
    & (Map.elems <$>)
    & (minimum <$>)
    & (fst <$>)

improve :: Handle -> Map.Map Char (Int, String) -> IO (Map.Map Char (Int, String))
improve h seen = do
  c <- hGetChar h
  if c == '\n' then return seen else improve h $ Map.mapWithKey (improve' c) seen
  where
    improve' c k (_, [])
      | k == toUpper c = (0, [])
      | otherwise = (1, [c])
    improve' c k (n, s@(x : xs))
      | k == toUpper c = (n, s)
      | eqOpCase c x = (n - 1, xs)
      | otherwise = (n + 1, c : s)

eqOpCase :: Char -> Char -> Bool
eqOpCase a b = (toUpper a == toUpper b) && ((isUpper a && isLower b) || (isLower a && isUpper b))
