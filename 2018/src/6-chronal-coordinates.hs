{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Function
import Data.Functor
import Data.Ix
import Data.List
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Semigroup
import Data.Void
import Debug.Trace
import System.IO
import Text.Megaparsec
import Text.Megaparsec.Char (newline, space, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Utils

type Parser = ParsecT Void String IO

parser :: (Integral a) => Parser (NonEmpty (Point a))
parser = liftA2 (:|) pointParser (pointParser `sepEndBy` space)
  where
    pointParser = Point <$> liftA2 (,) (decimal <* string ", ") (decimal <* newline)

newtype Point a = Point (a, a)

unwrap :: Point a -> (a, a)
unwrap (Point p) = p

instance (Ord a) => Semigroup (Point a) where
  Point (x, y) <> Point (x', y') = Point (max x x', max y y')

perim :: (Integral p, Ord p) => ((p, p), (p, p)) -> [(p, p)]
perim ((x0, y0), (x, y)) = [(x0,), (x,), (,y0), (,y)] <*> [(min x0 y0) .. (max x y)]

manhattan :: Num p => (p, p) -> (p, p) -> p
manhattan (x, y) (xp, yp) = abs (x - xp) + abs (y - yp)

main :: IO ()
main = do
  putStrLn "Input:"
  input <- unlines <$> readLines []
  Right points <- runParserT (parser @Int) "" input
  let bound = unwrap $ sconcat points
      pmap = Map.fromList $ zip [0 ..] (map unwrap $ toList points)
      p1 =
        runSTUArray (getFiniteAreas @Int pmap bound)
          & elems
          & maximum
      p2 = safeRegion 10000 (Map.elems pmap) bound
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2

getFiniteAreas :: (Integral p, Ord p, Ix p) => Map Int (p, p) -> (p, p) -> ST s (STUArray s Int Int)
getFiniteAreas pmap (\(x, y) -> ((0, 0), (x, y)) -> bnds) = do
  counts <- newArray (0, Map.size pmap - 1) 0
  forM_ (range bnds) $ inc counts . findClosest pmap
  forM_ (perim bnds) $ remove counts . findClosest pmap
  return counts
  where
    inc c Nothing = return ()
    inc c (Just i) = writeArray c i . succ =<< readArray c i
    remove c Nothing = return ()
    remove c (Just i) = writeArray c i 0

findClosest :: (Num p, Ord p) => Map Int (p, p) -> (p, p) -> Maybe Int
findClosest pmap p = case sortBy dist dists of
  (p0, d0) : (p1, d1) : _
    | d0 == d1 -> Nothing
    | otherwise -> Just p0
  where
    dist (_, d) (_, d') = compare d d'
    dists = Map.assocs $ Map.map (manhattan p) pmap

safeRegion :: (Num p, Ix p) => p -> [(p, p)] -> (p, p) -> Int
safeRegion r points (\(x, y) -> ((0, 0), (x, y)) -> bnds) =
  range bnds
    <&> manhattan
    <&> sum . (`map` points)
    & filter (< r)
    & length
