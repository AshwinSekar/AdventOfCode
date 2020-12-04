import Utils

import Control.Monad
import Control.Monad.ST

import Data.Function ((&))
import qualified Data.Set as Set
import Data.Map ((!))
import qualified Data.Map as Map

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (space, newline, string, letterChar)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = ParsecT Void String IO
type Policy a = (a, a, Char)

pswdParser :: (Num a) => Parser (Policy a, String)
pswdParser = do
  lo <- decimal
  hi <- between (string "-") space decimal
  char <- letterChar <* string ": "
  pswd <- many letterChar
  return ((lo, hi, char), pswd)

parser :: (Num a) => Parser [(Policy a, String)]
parser = pswdParser `sepEndBy1` space


valid :: (Policy Int, String) -> Bool
valid ((lo, hi, char), pswd) =
  lo <= occ && occ <= hi
  where occ = length $ filter (==char) pswd

valid2 :: (Policy Int, String) -> Bool
valid2 ((i, j, char), pswd) =
  (pswd !! (i - 1) == char) /= (pswd !! (j - 1) == char)

main :: IO ()
main = do
  putStrLn "Input:"
  input <- join <$> readLines []
  Right pswds <- runParserT parser "" input
  let p1 = length $ filter valid pswds
      p2 = length $ filter valid2 pswds
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
