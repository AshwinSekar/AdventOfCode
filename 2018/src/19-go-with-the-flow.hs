{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad
import Data.Bits
import Data.Function
import Data.Functor
import Data.List hiding ((\\))
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Data.Void
import Debug.Trace
import System.IO
import Text.Megaparsec
import Text.Megaparsec.Char (newline, space, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Utils

type Parser = ParsecT Void String IO

instrParser :: Parser Instr
instrParser = do
  op <-
    choice
      [ ADDR <$ string "addr",
        ADDI <$ string "addi",
        MULR <$ string "mulr",
        MULI <$ string "muli",
        BANR <$ string "banr",
        BANI <$ string "bani",
        BORR <$ string "borr",
        BORI <$ string "bori",
        SETR <$ string "setr",
        SETI <$ string "seti",
        GTIR <$ string "gtir",
        GTRI <$ string "gtri",
        GTRR <$ string "gtrr",
        EQIR <$ string "eqir",
        EQRI <$ string "eqri",
        EQRR <$ string "eqrr"
      ]
  [a, b, c] <- count 3 (space *> decimal)
  return $ Instr op a b c

parser :: Parser (Int, Map Int Instr)
parser =
  (,) <$> between (string "#ip ") newline decimal
    <*> (Map.fromList . zip [0 ..] <$> instrParser `sepEndBy1` newline)

type Registers = Map Int Int

type Prog = Map Int Instr

data Opcode
  = ADDR
  | ADDI
  | MULR
  | MULI
  | BANR
  | BANI
  | BORR
  | BORI
  | SETR
  | SETI
  | GTIR
  | GTRI
  | GTRR
  | EQIR
  | EQRI
  | EQRR
  deriving (Enum, Bounded, Eq, Show, Ord)

data Instr = Instr
  { op :: Opcode,
    a :: Int,
    b :: Int,
    c :: Int
  }
  deriving (Show, Eq)

main :: IO ()
main = do
  input <- readFile "data/19-puzzle-input"
  Right (ip, prog) <- runParserT parser "" input
  let r0 = Map.fromList $ zip [0 ..] [0, 0, 0, 0, 0, 0]
      r1 = eval prog ip r0
      r0' = Map.fromList $ zip [0 ..] [1, 0, 0, 0, 0, 0]
      r1' = eval prog ip r0'
  putStrLn $ "Part 1: " ++ show (r1 ! 0)
  putStrLn $ "Part 2: " ++ show (r1' ! 0)

eval :: Prog -> Int -> Registers -> Registers
eval prog ip r = case (i, Map.lookup i prog) of
  (_, Nothing) -> r
  (1, _) -> Map.insert 0 (sumFactors $ r ! 4) r
  (_, Just Instr {op, a, b, c}) -> eval prog ip $ Map.adjust (+ 1) ip (run op r a b c)
  where
    i = r ! ip

sumFactors :: Int -> Int
sumFactors n = sum $ filter isFactor [1 .. n]
  where
    isFactor j = n `rem` j == 0

run :: Opcode -> Registers -> Int -> Int -> Int -> Registers
run ADDR r a b c = Map.insert c (r ! a + r ! b) r
run ADDI r a b c = Map.insert c (r ! a + b) r
run MULR r a b c = Map.insert c (r ! a * r ! b) r
run MULI r a b c = Map.insert c (r ! a * b) r
run BANR r a b c = Map.insert c (r ! a .&. r ! b) r
run BANI r a b c = Map.insert c (r ! a .&. b) r
run BORR r a b c = Map.insert c (r ! a .|. r ! b) r
run BORI r a b c = Map.insert c (r ! a .|. b) r
run SETR r a _ c = Map.insert c (r ! a) r
run SETI r a _ c = Map.insert c a r
run GTIR r a b c = Map.insert c (test $ a > r ! b) r
run GTRI r a b c = Map.insert c (test $ r ! a > b) r
run GTRR r a b c = Map.insert c (test $ r ! a > r ! b) r
run EQIR r a b c = Map.insert c (test $ a == r ! b) r
run EQRI r a b c = Map.insert c (test $ r ! a == b) r
run EQRR r a b c = Map.insert c (test $ r ! a == r ! b) r

test :: Bool -> Int
test b = if b then 1 else 0
