import Utils

import Control.Applicative (liftA2)
import Control.Monad

import Data.Functor
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Tuple

import Data.Void
import Text.Megaparsec ((<|>), choice, someTill, try, sepBy1, sepEndBy)
import Text.Megaparsec.Char (letterChar, spaceChar)

type Adj = Map.Map String (Map.Map String Int)

letterSpace = letterChar <|> spaceChar

valueParser :: Parser (String, Int)
valueParser = swap <$> choice [
  try $ liftA2 (,) decimal (someTill letterSpace (symbol " bags")),
  liftA2 (,) decimal (someTill letterSpace (symbol " bag"))
  ]

ruleParser :: Parser (String, Map.Map String Int)
ruleParser =
  liftA2 (,) (someTill letterSpace (symbol " bags contain"))
             (choice [
                try $ Map.fromList <$> valueParser `sepBy1` symbol ",",
                symbol "no other bags" $> Map.empty ]
             )

parser :: Parser Adj
parser = Map.fromList <$> ruleParser `sepEndBy` symbol "."

bfs :: Adj -> Set.Set String -> Set.Set String -> Set.Set String
bfs adj v f
  | Set.null f = v
  | otherwise  = bfs adj (Set.union v f') f'
  where contains bg  = not . Set.null $ Set.intersection (Map.keysSet bg) f
        f' = Map.keysSet $ Map.filter contains adj
        v' = Set.union v f'

dfs :: Map.Map String Int -> Adj -> Map.Map String Int
dfs n = Map.map (Map.foldlWithKey (\ a k i -> a + i + i * (n ! k)) 0)

main :: IO ()
main = do
  adj <- parseFile "data/7-puzzle-input" parser
  let p1 = Set.size $ bfs adj Set.empty $ Set.singleton "shiny gold"
      memo = dfs memo adj
      p2 = memo ! "shiny gold"
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
