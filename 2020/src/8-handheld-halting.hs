import Utils

import Data.Function ((&))
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Void
import Text.Megaparsec (choice, some)

data Instr = Nop Int | Acc Int | Jmp Int
type Prog = Map.Map Int Instr

parser :: Parser Prog
parser = Map.fromList . zip [0,1..] <$> some instrParser
  where instrParser = choice [
          Nop <$> (symbol "nop " *> signed),
          Acc <$> (symbol "acc " *> signed),
          Jmp <$> (symbol "jmp " *> signed)
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
