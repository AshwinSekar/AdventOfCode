import           Control.Applicative (liftA2, (<|>))
import           Data.Function       ((&))
import           Data.Map            ((!))
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import           Text.Megaparsec     (some)
import           Utils

commandParser :: Parser (Int, Int)
commandParser =
  (, 0) <$> (symbol "forward" >> decimal) <|>
  (0, ) <$> (symbol "down" >> decimal) <|>
  (0, ) <$> (symbol "up" >> (0 -) <$> decimal)

main :: IO ()
main = do
  input <- parseFile "data/2-puzzle-input" $ some commandParser
  let (x, y) = foldl1 psum input
      p1 = x * y
      f (x, y, a) (d, 0) = (x + d, y + a * d, a)
      f (x, y, a) (0, d) = (x, y, a + d)
      (x', y', _) = foldl f (0, 0, 0) input
      p2 = x' * y'
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
