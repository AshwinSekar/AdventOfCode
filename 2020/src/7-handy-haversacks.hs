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
import Text.Megaparsec.Char (space, newline, string, letterChar, char, spaceChar)
import Text.Megaparsec.Char.Lexer (decimal)

import Debug.Trace

type Parser = ParsecT Void String IO
type Adj = Map.Map String (Map.Map String Int)

letterSpace = letterChar <|> spaceChar

valueParser :: Parser (String, Int)
valueParser = swap <$> choice [
  try $ liftA2 (,) (decimal <* space) (someTill letterSpace (string " bags")),
  liftA2 (,) (decimal <* space) (someTill letterSpace (string " bag"))
  ]

ruleParser :: Parser (String, Map.Map String Int)
ruleParser =
  liftA2 (,) (someTill letterSpace (string " bags contain "))
             (choice [
                try $ Map.fromList <$> valueParser `sepBy1` string ", ",
                string "no other bags" $> Map.empty
                ])

parser :: Parser Adj
parser = Map.fromList <$> ruleParser `sepEndBy` (char '.' *> newline)

bfs :: Adj -> Set.Set String -> Set.Set String -> Set.Set String
bfs adj v f
  | Set.null f = v
  | otherwise  = bfs adj (Set.union v f') f'
  where contains bg  = not . Set.null $ Set.intersection (Map.keysSet bg) f
        f' = Map.keysSet $ Map.filter contains adj
        v' = Set.union v f'

dfs :: Adj -> String -> State (Map.Map String Int) Int
dfs adj u = do
  cnts <- get
  case (Map.lookup u cnts, Map.lookup u adj) of
    (Just c, _)   -> return c
    (Nothing, Just (Map.assocs -> bg)) -> do
      c <- foldlM (\ t (k, v) -> (t + v +) <$> ((*v) <$> dfs adj k)) 0 bg
      modify (Map.insert u c)
      return c

main :: IO ()
main = do
  adj <- parseFile "data/7-puzzle-input" parser
  let p1 = Set.size $ bfs adj Set.empty $ Set.singleton "shiny gold"
      p2 = evalState (dfs adj "shiny gold") Map.empty
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
