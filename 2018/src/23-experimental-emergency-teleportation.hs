import Utils

import Data.Char
import Data.List
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char (space, newline, string)
import Text.Megaparsec.Char.Lexer (signed, decimal)

type Parser = Parsec Void String

parser :: (Num a) => Parser [Nano a]
parser = (`sepEndBy` newline) $ Nano
  <$> (string "pos=<" *> signed space decimal)
  <*> (string "," *> signed space decimal)
  <*> (string "," *> signed space decimal)
  <*> (string ">, r=" *> decimal)


data Nano a = Nano { x :: a
                   , y :: a
                   , z :: a
                   , r :: a} deriving (Show, Eq, Ord)

main :: IO ()
main = do
  putStrLn "Input:"
  input <- unlines <$> readLines []
  Just bots <- return $ parseMaybe (parser @Int) input
  putStrLn $ "Part 1: " ++ show (rangeL bots)

rangeL :: (Num a, Ord a) => [Nano a] -> Int
rangeL bots = length (filter test bots)
  where Nano x' y' z' r' = maximumBy (\ b b' -> compare (r b) (r b')) bots
        test Nano {x, y, z} = abs (x - x') + abs (y - y') + abs (z - z') <= r'
