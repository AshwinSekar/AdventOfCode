import Utils

import Control.Applicative (liftA2)
import Control.Monad
import Control.Monad.ST
import Control.Monad.State

import Data.Char
import Data.Function ((&))
import Data.Functor
import Data.List
import Data.Foldable
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Tuple

import Data.Void
import Text.Megaparsec (ParsecT, runParserT, (<|>), choice, someTill, try, sepBy1, sepEndBy)
import Text.Megaparsec.Char (space, newline, string, letterChar, char, spaceChar, printChar)
import Text.Megaparsec.Char.Lexer (decimal, signed)

import Debug.Trace

type Parser = ParsecT Void String IO
data Instr = Nop Int | Acc Int | Jmp Int
type Prog = Map.Map Int Instr

parser :: Parser Prog
parser = Map.fromList . zip [0,1..] <$> instrParser `sepEndBy` newline
  where instrParser = choice [
          Nop <$> (string "nop " *> signed space decimal),
          Acc <$> (string "acc " *> signed space decimal),
          Jmp <$> (string "jmp " *> signed space decimal)
         ]

run :: Set.Set Int -> Int -> Int -> Prog -> (Int, Bool)
run v i a prog =
  case (Set.member i v, Map.lookup i prog) of
    (True, _)         -> (a, False)
    (_, Nothing)      -> (a, True)
    (_, Just (Nop _)) -> run v' (i + 1) a prog
    (_, Just (Acc x)) -> run v' (i + 1) (a + x) prog
    (_, Just (Jmp x)) -> run v' (i + x) a prog
  where v' = Set.insert i v

terminate :: (Prog -> (Int, Bool)) -> Prog -> (Int, Bool)
terminate eval prog = map (\j -> eval $ Map.adjust adj j prog) [0..n]
                      & filter snd
                      & head
  where n = Map.size prog - 1
        adj (Nop x) = Jmp x
        adj (Jmp x) = Nop x
        adj x = x

main :: IO ()
main = do
  prog <- parseFile "data/8-puzzle-input" parser
  let (p1, False) = run Set.empty 0 0 prog
      (p2, True)  = terminate (run Set.empty 0 0) prog
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
