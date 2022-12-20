import           Control.Applicative (liftA2)
import           Data.Char           (isAsciiUpper, ord)
import           Data.Function       ((&))
import           Data.Functor        (($>))
import           Data.List           (intersect, sort)
import           Data.Map            ((!))
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import           Text.Megaparsec     (choice, some)
import           Utils               (Parser, getFile, slices, symbol)

priority :: Char -> Int
priority x
  | isAsciiUpper x = ord x - 38
priority x = ord x - 96

main :: IO ()
main = do
  rucksacks <- getFile "data/3-puzzle-input"
  let p1 =
        rucksacks &
        map
          (priority .
           head . uncurry intersect . (\s -> splitAt (length s `div` 2) s)) &
        sum
      p2 =
        slices 3 rucksacks &
        map (priority . head . (\[a, b, c] -> a `intersect` b `intersect` c)) &
        sum
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
