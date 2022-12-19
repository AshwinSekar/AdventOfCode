{-# LANGUAGE NamedFieldPuns #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import           Data.Array.IArray
import           Data.Array.ST
import           Data.Function
import           Data.Functor
import           Data.Ix
import           Data.List
import           Data.Void
import           Debug.Trace
import           System.IO
import           Text.Megaparsec
import           Text.Megaparsec.Char       (newline, space, string)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Utils

type Parser = ParsecT Void String IO

claimParser :: (Num a) => Parser (Claim a)
claimParser = do
  i <- between (string "#") (string " @ ") decimal
  (x, y) <- liftA2 (,) (decimal <* string ",") (decimal <* string ": ")
  (w, h) <- liftA2 (,) (decimal <* string "x") decimal
  return $ Claim i x y (x + w - 1) (y + h - 1)

parser :: (Num a) => Parser [Claim a]
parser = claimParser `sepEndBy1` space

data Claim a =
  Claim
    { iD   :: a
    , minX :: a
    , minY :: a
    , maxX :: a
    , maxY :: a
    }
  deriving (Show, Eq)

squares :: (Ix a) => Claim a -> [(a, a)]
squares Claim {minX, minY, maxX, maxY} = range ((minX, minY), (maxX, maxY))

main :: IO ()
main = do
  putStrLn "Input:"
  input <- join <$> readLines []
  Right claims <- runParserT parser "" input
  let counts = runSTUArray (getCounts claims ((0, 0), (1000, 1000)))
      p1 = length $ filter (>= 2) (elems counts)
      Just p2 = findUnique counts claims
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2

getCounts ::
     (Ix a) => [Claim a] -> ((a, a), (a, a)) -> ST s (STUArray s (a, a) Int)
getCounts claims bnds = do
  counts <- newArray bnds 0
  forM_ claims (\c -> forM_ (squares c) $ inc counts)
  return counts
  where
    inc c i = writeArray c i . succ =<< readArray c i

findUnique ::
     (Ix a, Integral e, IArray i e) => i (a, a) e -> [Claim a] -> Maybe a
findUnique counts claims = iD <$> find unique claims
  where
    unique c = all ((== 1) . (counts !)) $ squares c
