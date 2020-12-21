import Utils

import Control.Applicative (liftA2)
import Data.Foldable
import Data.Function ((&))
import Data.Functor
import Data.Map ((!))
import Data.Maybe
import Data.Void
import qualified Data.Map as Map
import Text.Megaparsec hiding (count)
import Text.Megaparsec.Char (letterChar, newline, char, string)
import qualified Text.Megaparsec.Char.Lexer as L (decimal)
import Text.Megaparsec.Debug

ruleParser :: Parser (Int, Either Char [[Int]])
ruleParser = liftA2 (,) (decimal <* char ':')
                        (choice [
                          Left  <$> between (string " \"") (char '\"') letterChar <* newline,
                          Right <$> some (someTill (char ' ' *> L.decimal) (string " |" <|> (newline $> [])))
                         ])

parser = liftA2 (,) (Map.fromList <$> some ruleParser <* newline)
                    (some letterChar `sepEndBy1` newline)

mkParser :: Map.Map Int (Parsec Void String ()) -> Either Char [[Int]] -> Parsec Void String ()
mkParser rulesP (Left c) = dbg "Lit" $ char c $> ()
mkParser rulesP (Right sub) = dbg "Pipe" (asum [traverse_ (rulesP !) s | s <- sub])


main :: IO ()
main = do
  (rules, msgs)  <- parseFile "data/19-puzzle-input" parser
  let rulesP  = Map.map (mkParser rulesP) rules
      rulesP' = Map.map (mkParser rulesP') $ Map.insert 8 (Right [[42], [42,8]]) (Map.insert 11 (Right [[42,31],[42,11,31]]) rules)
      p1 = count (isJust . parseMaybe (rulesP ! 0)) msgs
      p2 = filter (isJust . parseMaybe (rulesP' ! 0)) msgs
  print rulesP'
  -- putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
