{-# LANGUAGE LambdaCase, NamedFieldPuns #-}
import Utils

import Control.Monad

import Data.Bits
import Data.Function
import Data.Functor
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Void

import System.IO

import Text.Megaparsec
import Text.Megaparsec.Char (space, newline, string)
import Text.Megaparsec.Char.Lexer (signed, decimal)

type Parser = ParsecT Void String IO

parser :: Parser [Sample]
parser = (`sepEndBy` newline) $ Sample
  <$> (string "Before: [" *> regParser <* string "]" <* newline)
  <*> (decimal <* space) <*> (decimal <* space) <*> (decimal <* space) <*> (decimal <* newline)
  <*> (string "After:  [" *> regParser <* string "]" <* newline)

regParser :: Parser Registers
regParser = decimal `sepEndBy` string ", "
             <&> zip [0..]
             <&> Map.fromList

type Registers = Map Int Int
type Opcode = Registers -> Int -> Int -> Int -> Registers
data Sample = Sample { r :: Registers
                     , op :: Int
                     , a :: Int
                     , b :: Int
                     , c :: Int
                     , r' :: Registers
                     } deriving (Show, Eq)

main :: IO ()
main = do
  putStrLn "Input:"
  input <- readFile "data/16-puzzle-input"
  samples <- runParserT parser "" input >>= \case
    Left e -> putStrLn (errorBundlePretty e) >> return []
    Right s -> return s
  putStrLn $ "Part 1: " ++ show (behave samples 3)

behave :: [Sample] -> Int -> Int
behave samples n = map behave' samples
                    & filter ((>= n) . length)
                    & length

behave' :: Sample -> [Opcode]
behave' Sample{r,a,b,c,r'} = do
  op <- opcodes
  guard (op r a b c == r')
  return op


opcodes :: [Opcode]
opcodes = [addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr]

addr r a b c = Map.insert c (r ! a + r ! b) r
addi r a b c = Map.insert c (r ! a + b) r
mulr r a b c = Map.insert c (r ! a * r ! b) r
muli r a b c = Map.insert c (r ! a * b) r
banr r a b c = Map.insert c (r ! a .&. r ! b) r
bani r a b c = Map.insert c (r ! a .&. b) r
borr r a b c = Map.insert c (r ! a .|. r ! b) r
bori r a b c = Map.insert c (r ! a .|. b) r
setr r a _ c = Map.insert c (r ! a) r
seti r a _ c = Map.insert c a r
gtir r a b c = Map.insert c (test $ a > r ! b) r
gtri r a b c = Map.insert c (test $ r ! a > b) r
gtrr r a b c = Map.insert c (test $ r ! a > r ! b) r
eqir r a b c = Map.insert c (test $ a == r ! b) r
eqri r a b c = Map.insert c (test $ r ! a == b) r
eqrr r a b c = Map.insert c (test $ r ! a == r ! b) r

test :: Bool -> Int
test b = if b then 1 else 0
