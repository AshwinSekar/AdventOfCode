import           Control.Applicative (liftA2)
import           Data.Function       ((&))
import           Data.Functor        (($>))
import           Data.List           (sort)
import           Data.Map            ((!))
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import           Text.Megaparsec     (choice, some)
import           Utils               (Parser, parseFile, symbol)

data Hand
  = Rock
  | Paper
  | Scissors
  deriving (Eq, Enum, Show)

data Outcome
  = Loss
  | Draw
  | Win
  deriving (Eq, Enum, Show)

run :: Hand -> Hand -> Outcome
run Rock Paper = Win
run Paper Scissors = Win
run Scissors Rock = Win
run x y
  | x == y = Draw
run x y = Loss

score :: Hand -> Hand -> Int
score x y = 3 * fromEnum (run x y)

decode :: Hand -> Outcome -> Hand
decode x Draw       = x
decode Rock Win     = Paper
decode Paper Win    = Scissors
decode Scissors Win = Rock
decode x Loss       = decode (decode x Win) Win

handParser :: Parser Hand
handParser = choice [symbol "A" $> Rock, symbol "B" $> Paper, symbol "C" $> Scissors]

outcomeParser :: Parser Outcome
outcomeParser = choice [symbol "X" $> Loss, symbol "Y" $> Draw, symbol "Z" $> Win]

main :: IO ()
main = do
  handsAndOutcomes <- parseFile "data/2-puzzle-input" $ some (liftA2 (,) handParser outcomeParser)
  let hands = map (\(theirs, outcome) -> (theirs, toEnum $ fromEnum outcome)) handsAndOutcomes
      p1 = sum $ map (\(theirs, yours) -> fromEnum yours + 1 + score theirs yours) hands
      p2 =
        sum $
        map
          (\(hand, result) -> fromEnum (decode hand result) + 1 + 3 * fromEnum result)
          handsAndOutcomes
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
