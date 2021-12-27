module Utils
  ( readLines
  , splitString
  , slices
  , getFile
  , parseFile
  , count
  , fourDirs
  , eightDirs
  , eightDirs3D
  , eightDirs4D
  , dir
  , psum
  , psum3
  , psum4
  , gridMap
  , d2r
  , egcd
  , modInv
  , crt
  , lexeme
  , decimal
  , signed
  , symbol
  , Parser
  , xor
  , bin2dec
  ) where

import           Control.Monad
import qualified Data.Map                   as Map
import           Data.Void
import           Text.Megaparsec            (ParsecT, errorBundlePretty,
                                             runParserT)
import           Text.Megaparsec.Char       (char, space)
import qualified Text.Megaparsec.Char.Lexer as L (decimal, lexeme, signed,
                                                  symbol)

type Parser = ParsecT Void String IO

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

decimal :: (Num a) => Parser a
decimal = lexeme L.decimal

signed :: (Num a) => Parser a
signed = lexeme $ L.signed space L.decimal

symbol :: String -> Parser String
symbol = L.symbol space

fourDirs = [(0, 1), (1, 0), (0, -1), (-1, 0)]

eightDirs =
  [(0, 1), (1, 1), (1, 0), (0, -1), (-1, -1), (-1, 0), (-1, 1), (1, -1)]

eightDirs3D =
  [ (x, y, z)
  | x <- [-1 .. 1]
  , y <- [-1 .. 1]
  , z <- [-1 .. 1]
  , abs x + abs y + abs z > 0
  ]

eightDirs4D =
  [ (x, y, z, w)
  | x <- [-1 .. 1]
  , y <- [-1 .. 1]
  , z <- [-1 .. 1]
  , w <- [-1 .. 1]
  , abs x + abs y + abs z + abs w > 0
  ]

dir "N" = (1, 0)
dir "E" = (0, 1)
dir "S" = (-1, 0)
dir "W" = (0, -1)

readLines :: [String] -> IO [String]
readLines lines = do
  line <- getLine
  if null line
    then return $ reverse lines
    else readLines (line : lines)

getFile :: String -> IO [String]
getFile file = lines <$> readFile file

parseFile :: String -> ParsecT Void String IO a -> IO a
parseFile file parser = do
  input <- readFile file
  e <- runParserT parser "" input
  return $
    case e of
      Left x  -> error $ errorBundlePretty x
      Right x -> x

splitString :: (Char -> Bool) -> String -> [String]
splitString p s =
  case dropWhile p s of
    "" -> []
    s' -> w : splitString p s''
      where (w, s'') = break p s'

slices :: Int -> [a] -> [[a]]
slices size s =
  case splitAt size s of
    (x, []) -> [x]
    (x, y)  -> x : slices size y

count f = length . filter f

psum :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
psum (x, y) (x', y') = (x + x', y + y')

psum3 :: (Num a, Num b, Num c) => (a, b, c) -> (a, b, c) -> (a, b, c)
psum3 (x, y, z) (x', y', z') = (x + x', y + y', z + z')

psum4 (x, y, z, w) (x', y', z', w') = (x + x', y + y', z + z', w + w')

pprod :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
pprod (x, y) (x', y') = (x * x', y * y')

gridMap :: [[a]] -> Map.Map (Integer, Integer) a
gridMap input =
  Map.fromList . join $
  zipWith (\i s -> zipWith (\j c -> ((i, j), c)) [0 ..] s) [0 ..] input

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
  Right . (`mod` modPI) . sum . zipWith (*) crtModulii . zipWith (*) residues
  where
    modPI = product modulii
    crtModulii = (modPI `div`) <$> modulii

xor :: Bool -> Bool -> Bool
xor a b = a /= b

bin2dec :: [Int] -> Int
bin2dec = foldl (\a b -> 2 * a + b) 0
