import           Control.Applicative  (Alternative ((<|>)), liftA2)
import           Data.Char            (isAsciiUpper, ord)
import           Data.Complex         (Complex ((:+)), imagPart, realPart)
import           Data.Function        ((&))
import           Data.Functor         (($>))
import           Data.List            (find, intersect, sort)
import           Data.Map             ((!), (!?))
import qualified Data.Map             as Map
import           Data.Maybe           (mapMaybe)
import qualified Data.Set             as Set
import           Text.Megaparsec      (MonadParsec (try), choice, some)
import           Text.Megaparsec.Char (char, newline, printChar)
import           Utils                (Parser, count, decimal, getFile, lexeme,
                                       parseFile, slices, symbol, word)

type Worry = Int

type Monkey = Worry -> (Worry, Int)

type MonkeyMap = Map.Map Int (Monkey, [Worry])

lcmModulo :: Int
lcmModulo = 9699690 -- 96577

monkeyParser :: Bool -> Parser (Int, (Monkey, [Worry]))
monkeyParser div3 = do
  monkeyId <- symbol "Monkey" *> decimal <* symbol ":"
  items <-
    symbol "Starting items:" *> some (try (decimal <* symbol ",") <|> decimal)
  operation <-
    symbol "Operation: new = old" *>
    choice
      [ try $ symbol "*" $> (*) <*> decimal
      , symbol "* old" $> (^ 2)
      , symbol "+" $> (+) <*> decimal
      ]
  modulo <- symbol "Test: divisible by" *> decimal
  tMonkey <- symbol "If true: throw to monkey" *> decimal
  fMonkey <- symbol "If false: throw to monkey" *> decimal
  pure
    ( monkeyId
    , ( \worry ->
          let worry' =
                if div3
                  then operation worry `div` 3
                  else operation worry
           in ( worry' `mod` lcmModulo
              , if worry' `mod` modulo == 0
                  then tMonkey
                  else fMonkey)
      , items))

throw ::
     Int -> (Map.Map Int Int, MonkeyMap) -> Int -> (Map.Map Int Int, MonkeyMap)
throw n (count, monkeys) i =
  (Map.adjust (+ length items) monkeyId count, inspect monkeyId monkeys' items)
  where
    monkeyId = i `mod` n
    (monkey, items) = monkeys ! monkeyId
    monkeys' = Map.insert monkeyId (monkey, []) monkeys

inspect :: Int -> MonkeyMap -> [Worry] -> MonkeyMap
inspect monkeyId monkeys [] = monkeys
inspect monkeyId monkeys (item:items) = inspect monkeyId monkeys' items
  where
    (item', receiver) = fst (monkeys ! monkeyId) item
    monkeys' = Map.adjust (\(m, i) -> (m, i ++ [item'])) receiver monkeys

main :: IO ()
main = do
  monkeys1 <-
    Map.fromList <$> parseFile "data/11-puzzle-input" (some (monkeyParser True))
  monkeys2 <-
    Map.fromList <$>
    parseFile "data/11-puzzle-input" (some (monkeyParser False))
  let n = length monkeys1
      count = Map.fromList ((, 0) <$> [0 .. n - 1])
      (reverse . sort . Map.elems -> mb1, _) =
        foldl (throw n) (count, monkeys1) [0 .. (20 * n - 1)]
      (reverse . sort . Map.elems -> mb2, _) =
        foldl (throw n) (count, monkeys2) [0 .. (10000 * n - 1)]
      p1 = product $ take 2 mb1
      p2 = product $ take 2 mb2
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
