import           Control.Applicative  (liftA2)
import           Data.Char            (isAsciiUpper, ord)
import           Data.Function        ((&))
import           Data.Functor         (($>))
import           Data.List            (find, intersect, sort)
import           Data.Map             ((!), (!?))
import qualified Data.Map             as Map
import           Data.Maybe           (mapMaybe)
import qualified Data.Set             as Set
import           Text.Megaparsec      (choice, some)
import           Text.Megaparsec.Char (newline, printChar)
import           Utils                (Parser, count, decimal, getFile,
                                       parseFile, slices, symbol, word)

data Operation
  = Const Int
  | Add String String
  | Mul String String
  | Div String String
  | Sub String String
  deriving (Eq, Show)

operationParser :: Parser Operation
operationParser =
  choice
    [ Const <$> decimal
    , do s <- word
         choice
           [ Add <$> (s <$ symbol "+") <*> word
           , Mul <$> (s <$ symbol "*") <*> word
           , Div <$> (s <$ symbol "/") <*> word
           , Sub <$> (s <$ symbol "-") <*> word
           ]
    ]

monkeyParser :: Parser (String, Operation)
monkeyParser = liftA2 (,) word (symbol ":" *> operationParser)

main :: IO ()
main = do
  monkeys <-
    Map.fromList <$> parseFile "data/21-puzzle-input" (some monkeyParser)
  let solve _ (Const i)    = i
      solve vals (Add s t) = vals ! s + vals ! t
      solve vals (Mul s t) = vals ! s * vals ! t
      solve vals (Div s t) = vals ! s `div` vals ! t
      solve vals (Sub s t) = vals ! s - vals ! t
      vals = Map.map (solve vals) monkeys
      p1 = vals ! "root"
  putStrLn $ "Part 1: " ++ show p1
  -- putStrLn $ "Part 2: " ++ show p2
