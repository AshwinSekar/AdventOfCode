{-# LANGUAGE NamedFieldPuns #-}

import           Control.Monad
import           Data.Bits
import           Data.Function
import           Data.Functor
import           Data.List                  hiding ((\\))
import           Data.Map                   (Map, (!))
import qualified Data.Map                   as Map
import           Data.Set                   (Set, (\\))
import qualified Data.Set                   as Set
import           Data.Void
import           Debug.Trace
import           System.IO
import           Text.Megaparsec
import           Text.Megaparsec.Char       (newline, space, string)
import           Text.Megaparsec.Char.Lexer (decimal, signed)
import           Utils

type Parser = ParsecT Void String IO

sampleParser :: Parser Sample
sampleParser =
  Sample <$> between (string "Before: [") (string "]") regParser <* newline <*> instrParser <*
  newline <*>
  between (string "After:  [") (string "]") regParser <*
  newline

regParser :: Parser Registers
regParser = decimal `sepEndBy` string ", " <&> zip [0 ..] <&> Map.fromList

instrParser :: Parser Instr
instrParser = Instr <$> decimal <* space <*> decimal <* space <*> decimal <* space <*> decimal

parser :: Parser ([Sample], [Instr])
parser =
  (,) <$> sampleParser `sepEndBy1` newline <* many newline <*> instrParser `sepEndBy1` newline

type Registers = Map Int Int

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

data Instr =
  Instr
    { op :: Int
    , a  :: Int
    , b  :: Int
    , c  :: Int
    }
  deriving (Show, Eq)

data Sample =
  Sample
    { r     :: Registers
    , instr :: Instr
    , r'    :: Registers
    }
  deriving (Show, Eq)

main :: IO ()
main = do
  input <- readFile "data/16-puzzle-input"
  Right (samples, prog) <- runParserT parser "" input
  let eval r Instr {op, a, b, c} = run (findCodings samples ! op) r a b c
      r' = foldl eval (Map.fromList $ zip [0 ..] [0, 0, 0, 0]) prog
  putStrLn $ "Part 1: " ++ show (valid samples 3)
  putStrLn $ "Part 2: " ++ show (r' ! 0)

valid :: [Sample] -> Int -> Int
valid samples n = map behave samples & filter ((>= n) . length) & length

behave :: Sample -> [Opcode]
behave Sample {r, instr, r'} = do
  op <- [minBound .. maxBound]
  guard (run op r (a instr) (b instr) (c instr) == r')
  return op

findCodings :: [Sample] -> Map Int Opcode
findCodings samples = go Map.empty partial
  where
    behaves = map (\s -> (op $ instr s, Set.fromList $ behave s)) samples
    partial = Map.fromListWith Set.intersection behaves

go :: Map Int Opcode -> Map Int (Set Opcode) -> Map Int Opcode
go ops ints
  | Map.null ints = ops
  | otherwise = go ops' ints''
  where
    (dones, ints') = Map.partition ((== 1) . Set.size) ints
    ops' = Map.union ops $ Map.map (head . Set.toList) dones
    taken = Set.fromList $ Map.elems ops'
    ints'' = Map.map (\\ taken) ints'

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
test b =
  if b
    then 1
    else 0
