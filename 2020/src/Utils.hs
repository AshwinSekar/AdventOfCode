module Utils
( readLines,
  splitString,
  slices,
  getFile,
  parseFile,
  count,
  fourDirs,
  eightDirs,
  dir,
  psum,
  gridMap,
  d2r,
  egcd,
  modInv,
  crt
) where

import Control.Monad
import qualified Data.Map as Map
import Data.Void
import Text.Megaparsec (ParsecT, runParserT, errorBundlePretty)

fourDirs  = [(0, 1), (1, 0), (0, -1), (-1, 0)]
eightDirs = [(0, 1), (1, 1), (1, 0), (0, -1), (-1, -1), (-1, 0), (-1, 1), (1, -1)]

dir "N" = (1, 0)
dir "E" = (0, 1)
dir "S" = (-1, 0)
dir "W" = (0, -1)

readLines :: [String] -> IO [String]
readLines lines = do
  line <- getLine
  if null line
      then return $ reverse lines
      else readLines (line:lines)


getFile :: String -> IO [String]
getFile file = lines <$> readFile file

parseFile :: String -> ParsecT Void String IO a -> IO a
parseFile file parser = do
  input <- readFile file
  e <- runParserT parser "" input
  return $ case e of
    Left x  -> error $ errorBundlePretty x
    Right x -> x

splitString :: (Char -> Bool) -> String -> [String]
splitString p s = case dropWhile p s of
                  "" -> []
                  s' -> w : splitString p s''
                        where (w, s'') = break p s'

slices :: Int -> [a] -> [[a]]
slices size s = case splitAt size s of
                    (x, []) -> [x]
                    (x, y) -> x : slices size y

count f = length . filter f

psum :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
psum (x, y) (x', y') = (x + x', y + y')

pprod :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
pprod (x, y) (x', y') = (x * x', y * y')

gridMap :: [[a]] -> Map.Map (Integer, Integer) a
gridMap input = Map.fromList . join $ zipWith (\ i s -> zipWith (\j c -> ((i, j), c) ) [0..] s) [0..] input

d2r theta = pi * theta / 180

egcd :: Int -> Int -> (Int, Int)
egcd _ 0 = (1, 0)
egcd a b = (t, s - q * t)
  where
    (s, t) = egcd b r
    (q, r) = a `quotRem` b

modInv :: Int -> Int -> Either String Int
modInv a b =
  case egcd a b of
    (x, y)
      | a * x + b * y == 1 -> Right x
      | otherwise ->
        Left $ "No modular inverse for " ++ show a ++ " and " ++ show b

crt :: [Int] -> [Int] -> Either String Int
crt residues modulii =
  zipWithM modInv crtModulii modulii >>=
  (Right . (`mod` modPI) . sum . zipWith (*) crtModulii . zipWith (*) residues)
  where
    modPI = product modulii
    crtModulii = (modPI `div`) <$> modulii
