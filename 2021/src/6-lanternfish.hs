import Control.Applicative (liftA2, (<|>))
import Data.Function ((&))
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Megaparsec (sepBy1)
import Utils

main :: IO ()
main = do
  fish <- parseFile "data/6-puzzle-input" $ decimal `sepBy1` symbol ","
  let fishes = map (\x -> count (== x) fish) [0 .. 6]
      babies = take 9 [0, 0 ..]
      go (a : fish, b : baby) = (fish ++ [a + b], baby ++ [a + b])
      (f1, b1) = iterate go (fishes, babies) !! 80
      (f2, b2) = iterate go (fishes, babies) !! 256
  putStrLn $ "Part 1: " ++ show (sum f1 + sum b1)
  putStrLn $ "Part 2: " ++ show (sum f2 + sum b2)
