import           Control.Monad
import           Control.Monad.ST
import           Data.Function        ((&))
import           Data.Map             ((!))
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char (letterChar, space)
import           Utils

type Policy a = (a, a, Char)

pswdParser :: (Num a) => Parser (Policy a, String)
pswdParser = do
  lo <- decimal
  hi <- between (symbol "-") space decimal
  char <- letterChar <* symbol ": "
  pswd <- many letterChar
  return ((lo, hi, char), pswd)

parser :: (Num a) => Parser [(Policy a, String)]
parser = pswdParser `sepEndBy1` space

valid :: (Policy Int, String) -> Bool
valid ((lo, hi, char), pswd) = lo <= occ && occ <= hi
  where
    occ = length $ filter (== char) pswd

valid2 :: (Policy Int, String) -> Bool
valid2 ((i, j, char), pswd) = (pswd !! (i - 1) == char) /= (pswd !! (j - 1) == char)

main :: IO ()
main = do
  pswds <- parseFile "data/2-puzzle-input" parser
  let p1 = length $ filter valid pswds
      p2 = length $ filter valid2 pswds
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
