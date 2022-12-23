import           Control.Applicative  (liftA2)
import           Control.Monad.Extra  (whenM)
import           Data.Foldable
import           Data.Functor
import           Data.Map             ((!))
import qualified Data.Map             as Map
import           Data.Maybe
import           Data.Void
import           Text.Megaparsec      hiding (count)
import           Text.Megaparsec.Char (char, letterChar, newline)
import           Utils

ruleParser :: Parser (Int, Either Char [[Int]])
ruleParser =
  liftA2
    (,)
    (decimal <* symbol ":")
    (choice
       [ Left <$> between (symbol "\"") (symbol "\"") letterChar
       , Right <$> some (try $ decimal <* notFollowedBy (symbol ":")) `sepBy` symbol "|"
       ])

parser = liftA2 (,) (Map.fromList <$> some ruleParser) (some letterChar `sepEndBy1` newline)

mkParser :: Map.Map Int (Parsec Void String ()) -> Either Char [[Int]] -> Parsec Void String ()
mkParser rulesP (Left c)    = char c $> ()
mkParser rulesP (Right sub) = choice [try $ traverse_ (rulesP !) s | s <- sub]

main :: IO ()
main = do
  (rules, msgs) <- parseFile "data/19-puzzle-input" parser
  let rulesP = Map.map (mkParser rulesP) rules
      rule0 =
        whenM
          (liftA2 (<=) (length <$> some (try (rulesP ! 42))) (length <$> some (rulesP ! 31)))
          (fail "")
      p1 = count (isJust . parseMaybe (rulesP ! 0)) msgs
      p2 = count (isJust . parseMaybe rule0) msgs
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
