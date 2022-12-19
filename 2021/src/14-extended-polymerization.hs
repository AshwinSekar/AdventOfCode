import           Control.Applicative  (liftA2, (<|>))
import           Data.Bifunctor       (first, second)
import           Data.Function        ((&))
import           Data.Map             ((!), (!?))
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import           Text.Megaparsec      (some)
import           Text.Megaparsec.Char (letterChar, newline)
import           Utils

rulesParser :: Parser (Map.Map (Char, Char) Char)
rulesParser =
  Map.fromList <$>
  some
    (liftA2 (,) (liftA2 (,) letterChar letterChar) (symbol " -> " *> letterChar) <*
     newline)

main :: IO ()
main = do
  (init, rules) <-
    parseFile "data/14-puzzle-input" $
    liftA2 (,) (some letterChar <* newline) (newline *> rulesParser)
  let pairs = pairify init
      f = Map.foldrWithKey (polymerize rules) Map.empty
      p1 = iterate f pairs !! 10
      p2 = iterate f pairs !! 40
  putStrLn $ "Part 1: " ++ show (score (head init) p1)
  putStrLn $ "Part 1: " ++ show (score (head init) p2)

pairify :: String -> Map.Map (Char, Char) Int
pairify s = fst $ foldl pairify' (Map.empty, Nothing) s
  where
    pairify' (m, Nothing) c = (m, Just c)
    pairify' (m, Just b) c  = (Map.insertWith (+) (b, c) 1 m, Just c)

polymerize ::
     Map.Map (Char, Char) Char
  -> (Char, Char)
  -> Int
  -> Map.Map (Char, Char) Int
  -> Map.Map (Char, Char) Int
polymerize rules (b, c) cnt m =
  case rules !? (b, c) of
    Just new ->
      Map.insertWith (+) (b, new) cnt $ Map.insertWith (+) (new, c) cnt m
    Nothing -> Map.insertWith (+) (b, c) cnt m

score :: Char -> Map.Map (Char, Char) Int -> Int
score c pcounts = maximum counts - minimum counts
  where
    counts = Map.insertWith (+) c 1 $ Map.mapKeysWith (+) snd pcounts
