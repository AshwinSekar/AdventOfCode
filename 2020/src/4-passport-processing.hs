import           Control.Monad
import           Control.Monad.ST
import           Data.Char
import           Data.Function        ((&))
import           Data.Functor         ((<&>))
import           Data.List
import           Data.Map             ((!))
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char (asciiChar, lowerChar, newline, spaceChar)
import           Utils

type PPort = Map.Map String String

fields :: Set.Set String
fields = Set.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

eyeColors :: Set.Set String
eyeColors = Set.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

pairParser :: Parser (String, String)
pairParser = do
  key <- someTill lowerChar (symbol ":")
  val <- someTill asciiChar spaceChar
  return (key, val)

parser :: Parser PPort
parser = someTill pairParser newline >>= (return . Map.fromList)

valid :: PPort -> Bool
valid (Map.keysSet -> keys) = Set.null $ fields Set.\\ keys

validField :: String -> String -> Bool
validField "byr" (read -> yr) = yr >= 1920 && yr <= 2002
validField "iyr" (read -> yr) = yr >= 2010 && yr <= 2020
validField "eyr" (read -> yr) = yr >= 2020 && yr <= 2030
validField "ecl" clr = Set.member clr eyeColors
validField "pid" pid = length pid == 9 && all isDigit pid
validField "hcl" ('#':rest) = length rest == 6 && all isAlphaNum rest
validField "hgt" hgt =
  case ("cm" `isSuffixOf` hgt, "in" `isSuffixOf` hgt) of
    (True, _) -> 150 <= n && n <= 193
    (_, True) -> 59 <= n && n <= 76
    _         -> False
  where
    n = read $ dropWhileEnd isAlpha hgt
validField _ _ = False

main :: IO ()
main = do
  pports <- parseFile "data/4-puzzle-input" (some parser)
  let p1 = length $ filter valid pports
      p2 =
        filter valid pports &
        filter (\p -> Map.size (Map.filterWithKey validField p) == 7) &
        length
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
