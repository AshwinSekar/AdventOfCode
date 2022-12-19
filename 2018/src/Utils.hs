module Utils
  ( readLines,
    splitString,
    slices,
    kForMaxV,
    mapKWithV,
    (^+),
    (^*),
    (^-),
    (^$+),
    neigh,
    neigh',
    mapArrWithI,
  )
where

import Data.Array.IArray
import Data.Function
import Data.Ix
import qualified Data.Map as Map

readLines :: [String] -> IO [String]
readLines lines = do
  line <- getLine
  if null line
    then return $ reverse lines
    else readLines (line : lines)

splitString :: (Char -> Bool) -> String -> [String]
splitString p s = case dropWhile p s of
  "" -> []
  s' -> w : splitString p s''
    where
      (w, s'') = break p s'

slices :: Int -> [a] -> [[a]]
slices size s = case splitAt size s of
  (x, []) -> [x]
  (x, y) -> x : slices size y

kForMaxV :: (Ord k, Ord v) => Map.Map k v -> k
kForMaxV m =
  Map.filter (== maxM) m
    & Map.keys
    & head
  where
    maxM = maximum $ Map.elems m

mapKWithV :: (Ord k1, Ord k2) => (k1 -> a -> k2) -> Map.Map k1 a -> Map.Map k2 a
mapKWithV f mp = Map.mapKeys (\k -> f k (mp Map.! k)) mp

(^+) :: Num a => (a, a) -> (a, a) -> (a, a)
(i, j) ^+ (i', j') = (i + i', j + j')

(^*) :: Num a => a -> (a, a) -> (a, a)
x ^* (i, j) = (x * i, x * j)

(^-) :: Num a => (a, a) -> (a, a) -> (a, a)
(i, j) ^- (i', j') = (i - i', j - j')

(^$+) :: Num a => (a, a) -> a
(^$+) (i, j) = i + j

neigh :: Num a => (a, a) -> [(a, a)]
neigh (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

neigh' :: Num a => (a, a) -> [(a, a)]
neigh' (x, y) = (x + 1, y + 1) : (x + 1, y - 1) : (x - 1, y + 1) : (x - 1, y - 1) : neigh (x, y)

mapArrWithI :: (IArray t a, IArray t b, Ix i) => (i -> a -> b) -> t i a -> t i b
mapArrWithI f arr = array (bounds arr) [(i, f i (arr ! i)) | i <- indices arr]
