import Control.Applicative (liftA2)
import Data.List (sort)
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Megaparsec (some)
import Text.Megaparsec.Char (newline, space)
import Text.Megaparsec.Char.Lexer as L (decimal)
import Utils (parseFile)

main :: IO ()
main = do
  input <- parseFile "data/1-puzzle-input" $ some (some (L.decimal <* newline) <* space)
  let calories = reverse . sort $ map sum input
      p1 = head calories
      p2 = sum $ map (calories !!) [0, 1, 2]

  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2 " ++ show p2
