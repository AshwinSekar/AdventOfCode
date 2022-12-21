import           Control.Applicative  (liftA2)
import           Data.Char            (isAsciiUpper, ord)
import           Data.Complex         (Complex ((:+)), imagPart, realPart)
import           Data.Function        ((&))
import           Data.Functor         (($>))
import           Data.List            (find, intersect, sort)
import           Data.Map             ((!), (!?))
import qualified Data.Map             as Map
import           Data.Maybe           (mapMaybe)
import qualified Data.Set             as Set
import           GHC.Float            (double2Int)
import           Text.Megaparsec      (choice, some)
import           Text.Megaparsec.Char (newline, printChar)
import           Utils                (Parser, count, decimal, getFile,
                                       parseFile, signed, slices, symbol, word)

data Operation
  = Noop
  | Addx Int
  deriving (Eq, Show)

operationParser :: Parser Operation
operationParser =
  choice [symbol "noop" $> Noop, Addx <$> (symbol "addx" *> signed)]

calculate (cycle, x, vals) Noop = (cycle + 1, x, Map.insert cycle x vals)
calculate (cycle, x, vals) (Addx c) =
  (cycle + 2, x + c, Map.insert (cycle + 1) x $ Map.insert cycle x vals)

draw vals i =
  if abs (x - pos) <= 1
    then '⬜'
    else '⬛'
  where
    x = vals ! i
    pos = (i - 1) `mod` 40

main :: IO ()
main = do
  operations <- parseFile "data/10-puzzle-input" (some operationParser)
  let (_, _, vals) = foldl calculate (1, 1, Map.empty) operations
      p1 = sum $ map (\c -> c * vals ! c) [20,60 .. 240]
      p2 = unlines $ slices 40 $ map (draw vals) [1 .. 240]
  putStrLn $ "Part 1: " ++ show p1
  putStrLn "Part 2: "
  putStrLn p2
