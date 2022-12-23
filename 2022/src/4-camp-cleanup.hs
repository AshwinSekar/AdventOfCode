import           Control.Applicative (liftA2)
import           Data.Char           (isAsciiUpper, ord)
import           Data.Function       ((&))
import           Data.Functor        (($>))
import           Data.List           (intersect, sort)
import           Data.Map            ((!))
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import           Text.Megaparsec     (choice, some)
import           Utils               (Parser, count, decimal, getFile, parseFile, slices, symbol)

rangeParser = liftA2 (,) decimal (symbol "-" *> decimal)

main :: IO ()
main = do
  ranges <-
    parseFile "data/4-puzzle-input" $ some (liftA2 (,) rangeParser (symbol "," *> rangeParser))
  let p1 = count (\((a, b), (c, d)) -> a >= c && b <= d || c >= a && d <= b) ranges
      p2 = count (\((a, b), (c, d)) -> not (b < c || d < a)) ranges
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
